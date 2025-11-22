
module Streamly.Matcher (
                          runMatcher
                        , updatePrunable
                        )
where
import           Constants                              (labels, pathMap)
import           Control.Concurrent.STM                 (atomically, putTMVar, readTVar,
                                                         writeTVar)
import           Control.Exception                      (IOException, try)
import           Control.Monad                          (void)
import           Data.Function                          ((&))
import           Data.HashMap.Strict                    (HashMap)
import qualified Data.HashMap.Strict                    as HM (delete,
                                                               findWithDefault,
                                                               insert,
                                                               insertWith,
                                                               lookup)
import           Data.HashSet                           (HashSet)
import qualified Data.HashSet                           as HS (delete, insert,
                                                               intersection,
                                                               null)
import qualified Data.IntMap                            as IM (filter,
                                                               findWithDefault,
                                                               insert, keys,
                                                               member)
import           Data.IntMap.Strict                     (IntMap)
import           Data.IntSet                            (IntSet)
import qualified Data.IntSet                            as IS (difference,
                                                               foldr, fromList,
                                                               insert,
                                                               singleton, union)
import           Data.List                              (intersect)
import           Data.Maybe                             (fromJust)
import qualified Streamly.Data.Fold                     as F (foldlM')
import           Streamly.Data.Fold                     (Fold)
import qualified Streamly.Data.Stream                   as S (fold, filterM)
import           Streamly.Data.Stream                   (Stream)
import           Streamly.Data.Stream.Prelude           (eager, parList)
import qualified Streamly.Data.Fold.Prelude as F (parBuffered)
import qualified Streamly.Internal.FileSystem.PosixPath as FP (toString)
import           Streamly.Internal.FS.Event.Linux       (Event, getAbsPath,
                                                         isCreated, isDeleted,
                                                         isDir, isEventsLost)
import qualified Transmission.RPC.Torrent               as T (labels)
import           Transmission.RPC.Torrent               (Torrent, progress,
                                                         toId)
import           Types                                  (Matcher,
                                                         UFID (UFID),
                                                         UpdateEvent (FSEvent, RPCEvent),
                                                         arrIDs, arrIDsMap,
                                                         arrs,
                                                         idToTorrents, prunable,
                                                         torrentToIDs, prunableReady)
import           Utils                                  (buildFilesPath,
                                                         getFileNodes,
                                                         mkPathMap)
import Control.Concurrent (forkIO)
import System.Posix.ByteString (FileStatus, getFileStatus, deviceID, fileID)
import Data.ByteString.Char8 as BS (pack)

-- | Merge several 'UpdateEvent' streams (typically: filesystem events,
--   RPC events, UI events) and process them concurrently using up to
--   @maxThreads@ from the supplied 'Matcher'.
matcher :: Matcher-> [Stream IO UpdateEvent] -> IO ()
matcher mt streams = oneStream & S.fold (consumer mt)
  where
    oneStream = parList (eager True) streams

consumer :: Matcher -> Fold IO UpdateEvent ()
consumer mt = F.parBuffered id . F.foldlM' consume $ pure ()
  where
    consume _ ev = case ev of
                     RPCEvent torStream broadcaster -> do
                        torStream & S.filterM (needed mt)
                          & S.fold (F.parBuffered id $ F.foldlM' (\_ t -> updateNewTorrents t mt) $ pure ())
                        atomically $ do
                          p <- readTVar . prunable $ mt
                          putTMVar broadcaster p
                     FSEvent e -> do
                       updateNewFiles e mt
                       pure ()


needed :: Matcher -> Torrent -> IO Bool
needed m t = atomically $ do
  tid <- readTVar . torrentToIDs $ m
  -- Only update if torrent is complete, has relevant labels, and is not already in the mapping
  pure . not $ ((fromJust . toId $ t) `IM.member` tid || progress t /= Just 100 || null ((fromJust. T.labels $ t) `intersect` labels))

runMatcher :: Matcher -> [Stream IO UpdateEvent] -> IO ()
runMatcher mt streams = do
  void . forkIO $ buildArrs mt
  matcher mt streams

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
updateNewTorrents ::  Torrent -> Matcher -> IO ()
updateNewTorrents t m = do
  -- Get the list of filesystem nodes associated with this torrent
  nodelist <- fmap fst . getFileNodes . buildFilesPath (mkPathMap pathMap) $ t

  atomically $ do
    -- Read current torrent-to-files and file-to-torrent mappings
    tid <- readTVar . torrentToIDs $ m
    itd <- readTVar . idToTorrents $ m

    -- Update the mappings with the new torrent
    let (tid', itd') = updateTorrentMaps t nodelist tid itd
    writeTVar (torrentToIDs m) tid'
    writeTVar (idToTorrents m) itd'

    -- Read current set of known files and prunable torrents
    ais <- readTVar . arrIDs $ m
    p <- readTVar . prunable $ m

    -- Update prunable set based on new torrent’s files
    writeTVar (prunable m) $ updatePrunable (fromJust . toId $ t) tid' ais p

-- | Handle a filesystem event: creation, deletion or lost-events notice.
--
--   For file creation, the file is probed using 'getFileStatus' and its
--   UFID inserted into matcher state.
--   For deletion, UFIDs and reverse indexes are updated.
--   When events are lost, a full rebuild of the filesystem index is performed.
updateNewFiles :: Event -> Matcher -> IO ()
updateNewFiles ev m
  | isDir ev = pure ()  -- Only track files, not directories, as they can't be hardlinked
  | isCreated ev = do
      let fp = BS.pack . FP.toString . getAbsPath $ ev
      -- Safely attempt to get file status; ignore missing files
      es <- try (getFileStatus fp) :: IO (Either IOException FileStatus)
      case es of
        Left _ -> pure ()  -- File disappeared between event and status read
        Right s -> do
          let i = fileID s
              d = deviceID s
              u = UFID (i, d)

          atomically $ do
            -- Insert new file into global arrIDs and arrIDsMap
            ais <- readTVar . arrIDs $ m
            aim <- readTVar . arrIDsMap $ m
            let ais' = HS.insert u ais
                aim' = HM.insert fp u aim
            writeTVar (arrIDs m) ais'
            writeTVar (arrIDsMap m) aim'

            -- Remove affected torrents from prunable set
            idt <- readTVar . idToTorrents $ m
            let tis = HM.findWithDefault mempty u idt
            p <- readTVar (prunable m)
            writeTVar (prunable m) $ p `IS.difference` tis
  | isDeleted ev = do
      let fp = BS.pack . FP.toString . getAbsPath $ ev

      atomically $ do
        -- Look up the deleted file in arrIDsMap
        aim <- readTVar . arrIDsMap $ m
        case HM.lookup fp aim of
          Nothing -> pure ()
          Just i -> do
            -- Remove file from arrIDs and arrIDsMap
            ais <- readTVar . arrIDs $ m
            let ais' = HS.delete i ais
                aim' = HM.delete fp aim
            writeTVar (arrIDs m) ais'
            writeTVar (arrIDsMap m) aim'

            -- Recalculate prunable torrents affected by this deletion
            idt <- readTVar . idToTorrents $ m
            tid <- readTVar . torrentToIDs $ m
            case HM.lookup i idt of
              Nothing -> pure ()
              Just tos -> do
                p <- readTVar . prunable $ m
                let p' = IS.foldr (updatePrunableDeletion ais tid) p tos
                writeTVar (prunable m) p'
  | isEventsLost ev = buildArrs m
      -- On lost events, rebuild arrIDs and arrIDsMap completely
  | otherwise = pure ()

buildArrs :: Matcher -> IO ()
buildArrs m = do
      let as = arrs m
          pr = prunableReady m
      atomically . writeTVar pr $ False
      (ais, aim) <- getFileNodes as
      atomically $ do
        writeTVar (arrIDs m) ais
        writeTVar (arrIDsMap m) aim

        -- Recalculate prunable torrents for missing files
        tid <- readTVar . torrentToIDs $ m
        let p = IS.fromList . IM.keys . IM.filter (\fs -> null (fs `HS.intersection` ais)) $ tid
        writeTVar (prunable m) p
        writeTVar pr True

-- | Update both the forward and reverse maps when a new torrent arrives.
--
--   * Inserts @torrentID -> set UFID@ into the forward IntMap.
--   * Inserts @UFID -> set torrentID@ into the reverse HashMap.
--
--   Returns the updated pair.
updateTorrentMaps :: Torrent -> HashSet UFID -> IntMap (HashSet UFID) -> HashMap UFID IntSet -> (IntMap (HashSet UFID), HashMap UFID IntSet)
updateTorrentMaps to nodelist tid itd = (updateIm, updateHm)
  where
    ti = fromJust . toId $ to
    updateIm = IM.insert ti nodelist tid
    updateHm = foldr (\fid -> HM.insertWith IS.union fid (IS.singleton ti)) itd nodelist

-- | Update the prunable set after adding a finished torrent.
--
--   A torrent becomes prunable if none of its UFIDs intersect the current
--   active UFID set ('arrIDs').
updatePrunable :: Int -> IntMap (HashSet UFID) -> HashSet UFID -> IntSet -> IntSet
updatePrunable t tid ais p = if HS.null cross then IS.insert t p else p
  where
    tids = IM.findWithDefault mempty t tid
    cross = tids `HS.intersection` ais

-- | Update the prunable set on deletion events.
--
--   When a UFID is removed from the filesystem, each torrent mapped from
--   that UFID is re-checked. Torrents whose remaining UFIDs have no
--   filesystem match become prunable.
updatePrunableDeletion :: HashSet UFID -> IntMap (HashSet UFID) -> Int -> IntSet -> IntSet
updatePrunableDeletion ais tid ti p
  | null (tids `HS.intersection` ais) = IS.insert ti p
  | otherwise = p
  where
    tids = IM.findWithDefault mempty ti tid
