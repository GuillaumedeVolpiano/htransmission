module Streamly.Matcher (
                          runMatcher
                        , updatePrunable
                        , buildArrs
                        )
where
import           Constants                              (labels, pathMap)
import           Control.Concurrent.STM                 (atomically, putTMVar,
                                                         readTVar, writeTVar)
import           Control.Exception                      (IOException, try)
import           Control.Monad.IO.Class                 (MonadIO (liftIO))
import           Data.ByteString.Char8                  as BS (pack)
import           Data.Function                          ((&))
import qualified Data.HashMap.Strict                    as HM (delete, insert,
                                                               lookup, size)
import qualified Data.IntMap                            as IM (filter,
                                                               findWithDefault,
                                                               foldrWithKey,
                                                               insert, keys,
                                                               member, foldl')
import           Data.IntMap.Strict                     (IntMap)
import           Data.IntSet                            (IntSet)
import qualified Data.IntSet                            as IS (difference,
                                                               foldr, fromList,
                                                               insert, member,
                                                               size)
import           Data.List                              (intersect)
import           Data.Maybe                             (fromJust)
import qualified Data.Text                              as T (pack)
import           Log                                    (logTrace_)
import           Log.Monad                              (LogT)
import qualified Streamly.Data.Fold                     as F (foldlM')
import           Streamly.Data.Fold                     (Fold)
import qualified Streamly.Data.Stream                   as S (filterM, fold)
import           Streamly.Data.Stream                   (Stream)
import           Streamly.Data.Stream.Prelude           (eager, parList)
import qualified Streamly.Data.Stream.Prelude           as S (parBuffered)
import qualified Streamly.Internal.FileSystem.PosixPath as FP (toString)
import           Streamly.Internal.FS.Event.Linux       (Event, getAbsPath,
                                                         isCreated, isDeleted,
                                                         isDir, isEventsLost)
import           System.Posix.ByteString                (FileStatus, deviceID,
                                                         fileID, getFileStatus)
import qualified Transmission.RPC.Torrent               as T (labels)
import           Transmission.RPC.Torrent               (Torrent, progress,
                                                         toId)
import           Types                                  (Matcher,
                                                         UpdateEvent (FSEvent, RPCEvent),
                                                         arrIDs, arrIDsMap,
                                                         arrs, idToTorrents,
                                                         prunable,
                                                         prunableReady,
                                                         torrentToIDs)
import           Utils                                  (buildFilesPath,
                                                         deleteFromUFIDPseudoSet,
                                                         findWithDefaultUFIDPseudoMap,
                                                         getFileNodes,
                                                         insertInUFIDPseudoMap,
                                                         insertInUFIDPseudoSet,
                                                         lookupUFIDPseudoMap,
                                                         mkPathMap, mkUfid,
                                                         timeIt,
                                                         ufidPseudoSetIntersection)

-- | Merge several 'UpdateEvent' streams (typically: filesystem events,
--   RPC events, UI events) and process them concurrently using up to
--   @maxThreads@ from the supplied 'Matcher'.
matcher :: Matcher-> [Stream (LogT IO) UpdateEvent] -> LogT IO ()
matcher mt streams = oneStream & S.fold (consumer mt)
  where
    oneStream = parList (eager True) streams

consumer :: Matcher -> Fold (LogT IO) UpdateEvent ()
consumer mt = F.foldlM' consume $ pure ()
  where
    consume :: () -> UpdateEvent -> LogT IO ()
    consume _ ev = case ev of
                     RPCEvent torStream broadcaster -> do
                        ((tid, itd, ais, pr), t1) <- timeIt . liftIO . atomically $ do
                           t <- readTVar . torrentToIDs $ mt
                           i <- readTVar . idToTorrents $ mt
                           a <- readTVar . arrIDs $ mt
                           p <- readTVar . prunable $ mt
                           pure (t, i, a, p)
                        logTrace_ . T.pack $ "RPCEvent TVar reading took " ++ show t1 ++ " ms"
                        ((tid', itd', pr'), t2) <- timeIt $ S.parBuffered (eager True) torStream & S.filterM (needed mt)
                          & S.fold (F.foldlM' (updateNewTorrents ais)
                            $ pure (tid, itd, pr))
                        logTrace_ . T.pack $ "RPCEvent torrents fold took " ++ show t2 ++ " ms"
                        (_, t3) <- timeIt . liftIO . atomically $ do
                          writeTVar (torrentToIDs mt) tid'
                          writeTVar (idToTorrents mt) itd'
                          writeTVar (prunable mt) pr'
                          putTMVar broadcaster pr'
                        logTrace_ . T.pack $ "RPCEevent TVar writing took " ++ show t3 ++ " ms"
                        pure ()
                     FSEvent e -> do
                       updateNewFiles e mt
                       pure ()


needed :: Matcher -> Torrent -> LogT IO Bool
needed m t = liftIO . atomically $ do
  tid <- readTVar . torrentToIDs $ m
  -- Only update if torrent is complete, has relevant labels, and is not already in the mapping
  pure . not $ ((fromJust . toId $ t) `IM.member` tid || progress t /= Just 100 || null ((fromJust. T.labels $ t) `intersect` labels))

runMatcher :: Matcher -> [Stream (LogT IO) UpdateEvent] -> LogT IO ()
runMatcher = matcher

-- | Handle a single 'UpdateEvent'.
--
--   * 'RPCEvent t' dispatches to torrent-update logic.
--   * 'End broadcaster' writes the accumulated prunable set.
--   * 'FSEvent ev' dispatches to filesystem logic.
-- | Handle arrival of a new finished torrent.
--
--   Computes the set of filesystem node IDs for the torrent, updates
--   all matcher maps ('torrentToIDs', 'idToTorrents'), and recomputes the
--   prunable set.
--
--   The lookup and update of internal matcher state is done atomically.
updateNewTorrents ::  IntMap IntSet
                      -> ( IntMap (IntMap IntSet), IntMap (IntMap IntSet)
                          , IntSet )
                      -> Torrent
                      -> LogT IO ( IntMap (IntMap IntSet), IntMap (IntMap IntSet)
                            , IntSet )
updateNewTorrents ais (tid, itd, pr) t = do
  -- Get the list of filesystem nodes associated with this torrent
  nodelist <- fmap fst . liftIO . getFileNodes . buildFilesPath (mkPathMap pathMap) $ t
  let ti = fromJust . toId $ t
      (tid', itd') = updateTorrentMaps ti nodelist tid itd
      pr' = updatePrunable ti tid' ais pr
  pure (tid', itd', pr')


-- | Handle a filesystem event: creation, deletion or lost-events notice.
--
--   For file creation, the file is probed using 'getFileStatus' and its
--   UFID inserted into matcher state.
--   For deletion, UFIDs and reverse indexes are updated.
--   When events are lost, a full rebuild of the filesystem index is performed.
updateNewFiles :: Event -> Matcher -> LogT IO ()
updateNewFiles ev m
  | isDir ev = pure ()  -- Only track files, not directories, as they can't be hardlinked
  | isCreated ev = do
      let fp = BS.pack . FP.toString . getAbsPath $ ev
      -- Safely attempt to get file status; ignore missing files
      es <- liftIO . try $ getFileStatus fp :: LogT IO (Either IOException FileStatus)
      case es of
        Left _ -> pure ()  -- File disappeared between event and status read
        Right s -> do
          let i = fileID s
              d = deviceID s
              u = mkUfid i d

          liftIO . atomically $ do
            -- Insert new file into global arrIDs and arrIDsMap
            ais <- readTVar . arrIDs $ m
            aim <- readTVar . arrIDsMap $ m
            let ais' = insertInUFIDPseudoSet u ais
                aim' = HM.insert fp u aim
            writeTVar (arrIDs m) ais'
            writeTVar (arrIDsMap m) aim'

            -- Remove affected torrents from prunable set
            idt <- readTVar . idToTorrents $ m
            let tis = findWithDefaultUFIDPseudoMap mempty u idt
            p <- readTVar (prunable m)
            writeTVar (prunable m) $ p `IS.difference` tis
  | isDeleted ev = do
      let fp = BS.pack . FP.toString . getAbsPath $ ev

      liftIO . atomically $ do
        -- Look up the deleted file in arrIDsMap
        aim <- readTVar . arrIDsMap $ m
        case HM.lookup fp aim of
          Nothing -> pure ()
          Just i -> do
            -- Remove file from arrIDs and arrIDsMap
            ais <- readTVar . arrIDs $ m
            let ais' = deleteFromUFIDPseudoSet i ais
                aim' = HM.delete fp aim
            writeTVar (arrIDs m) ais'
            writeTVar (arrIDsMap m) aim'

            -- Recalculate prunable torrents affected by this deletion
            idt <- readTVar . idToTorrents $ m
            tid <- readTVar . torrentToIDs $ m
            case lookupUFIDPseudoMap i idt of
              Nothing -> pure ()
              Just tos -> do
                p <- readTVar . prunable $ m
                let p' = IS.foldr (updatePrunableDeletion ais' tid) p tos
                writeTVar (prunable m) p'
  | isEventsLost ev = do
      buildArrs m

      -- On lost events, rebuild arrIDs and arrIDsMap completely
  | otherwise = pure ()

buildArrs :: Matcher -> LogT IO ()
buildArrs m = do
      let as = arrs m
          pr = prunableReady m
      tid <- liftIO . atomically $ do
          writeTVar pr False
          readTVar . torrentToIDs $ m
      (((ais, aim), (ss, ms)), t) <- timeIt $ do
        (s, p) <- liftIO $ getFileNodes as
        let mapSize = IM.foldl' (\a e -> IS.size e + a) 0
            ss' = mapSize s
            ms' = HM.size p
        pure ((s, p), (ss', ms'))
      logTrace_ . T.pack $ "buildArrs took " ++ show t ++ " ms. " ++ show ss ++ " inodes and " ++ show ms ++ " paths found."
      let p = IS.fromList . IM.keys . IM.filter (\fs -> null (fs `ufidPseudoSetIntersection` ais)) $ tid
      liftIO . atomically $ do
              writeTVar (arrIDs m) ais
              writeTVar (arrIDsMap m) aim
              -- Recalculate prunable torrents for missing files
              writeTVar (prunable m) p
              writeTVar pr True

-- | Update both the forward and reverse maps when a new torrent arrives.
--
--   * Inserts @torrentID -> set UFID@ into the forward IntMap.
--   * Inserts @UFID -> set torrentID@ into the reverse HashMap.
--
--   Returns the updated pair.
updateTorrentMaps :: Int -> IntMap IntSet -> IntMap (IntMap IntSet) -> IntMap (IntMap IntSet) -> (IntMap (IntMap IntSet), IntMap (IntMap IntSet))
updateTorrentMaps ti nodelist tid itd = (updateIm, updateHm)
  where
    updateIm = IM.insert ti nodelist tid
    updateHm = insertInUFIDPseudoMap nodelist ti itd

-- | Update the prunable set after adding a finished torrent.
--
--   A torrent becomes prunable if none of its UFIDs intersect the current
--   active UFID set ('arrIDs').
updatePrunable :: Int -> IntMap (IntMap IntSet) -> IntMap IntSet -> IntSet -> IntSet
updatePrunable t tid ais p = if foundUFID then p else IS.insert t p
  where
    tids = IM.findWithDefault mempty t tid
    foundUFID = IM.foldrWithKey checkDevID False tids
    checkDevID devId fileIds short
      | short = True
      | otherwise = hasCommon fileIds $ IM.findWithDefault mempty devId ais
    hasCommon a = IS.foldr (shortMember a) True
    shortMember a b c = c || b `IS.member` a

-- | Update the prunable set on deletion events.
--
--   When a UFID is removed from the filesystem, each torrent mapped from
--   that UFID is re-checked. Torrents whose remaining UFIDs have no
--   filesystem match become prunable.
updatePrunableDeletion :: IntMap IntSet -> IntMap (IntMap IntSet) -> Int -> IntSet -> IntSet
updatePrunableDeletion ais tid ti = updatePrunable ti tid ais
