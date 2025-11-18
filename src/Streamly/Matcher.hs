
module Streamly.Matcher (matcher)

where
import           Constants                              (labels, pathMap)
import           Control.Concurrent.STM                 (atomically, putTMVar,
                                                         readTVar, readTVarIO,
                                                         writeTVar)
import           Control.Exception                      (IOException, try)
import           Control.Monad                          (unless)
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
import qualified Streamly.Data.Stream                   as S (mapM)
import           Streamly.Data.Stream                   (Stream)
import           Streamly.Data.Stream.Prelude           (eager, maxThreads,
                                                         parList)
import qualified Streamly.Internal.FileSystem.PosixPath as FP (toString)
import           Streamly.Internal.FS.Event.Linux       (Event, getAbsPath,
                                                         isCreated, isDeleted,
                                                         isDir, isEventsLost)
import           System.Posix                           (FileStatus, deviceID,
                                                         fileID, getFileStatus)
import qualified Transmission.RPC.Torrent               as T (labels)
import           Transmission.RPC.Torrent               (Torrent, progress,
                                                         toId)
import qualified Types                                  as T (Matcher (maxThreads))
import           Types                                  (Matcher,
                                                         Response (End, T),
                                                         UFID (UFID),
                                                         UpdateEvent (FSEvent, RPCEvent),
                                                         arrIDs, arrIDsMap,
                                                         arrs, idToTorrents,
                                                         prunable, torrentToIDs)
import           Utils                                  (buildFilesPath,
                                                         getFileNodes,
                                                         mkPathMap)

-- | Merge several 'UpdateEvent' streams (typically: filesystem events,
--   RPC events, UI events) and process them concurrently using up to
--   @maxThreads@ from the supplied 'Matcher'.  
matcher :: Matcher-> [Stream IO UpdateEvent] -> Stream IO ()
matcher mt streams = S.mapM (match mt) oneStream
  where
    oneStream = parList (maxThreads (T.maxThreads mt) . eager True) streams

-- | Handle a single 'UpdateEvent'.  
--  
--   * 'RPCEvent t' dispatches to torrent-update logic.  
--   * 'End broadcaster' writes the accumulated prunable set.  
--   * 'FSEvent ev' dispatches to filesystem logic.
match :: Matcher -> UpdateEvent -> IO ()
match m u = case u of
              RPCEvent r -> do
                case r of
                  T t -> updateNewTorrents t m
                  End broadcaster -> do
                    p <- readTVarIO . prunable $ m
                    atomically . putTMVar broadcaster $ p
              FSEvent ev -> do
                updateNewFiles ev m
                pure  ()

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
  nodelist <- fmap fst . getFileNodes (T.maxThreads m) . buildFilesPath (mkPathMap pathMap) $ t

  atomically $ do
    -- Read current torrent-to-files and file-to-torrent mappings
    tid <- readTVar . torrentToIDs $ m
    itd <- readTVar . idToTorrents $ m

    -- Only update if torrent is complete, has relevant labels, and is not already in the mapping
    unless ((fromJust . toId $ t) `IM.member` tid || progress t /= Just 100 || null ((fromJust. T.labels $ t) `intersect` labels)) $ do
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
      let fp = FP.toString . getAbsPath $ ev
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
      let fp = FP.toString . getAbsPath $ ev

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

  | isEventsLost ev = do
      -- On lost events, rebuild arrIDs and arrIDsMap completely
      let as = arrs m
      (ais, aim) <- getFileNodes (T.maxThreads m) as
      atomically $ do
        writeTVar (arrIDs m) ais
        writeTVar (arrIDsMap m) aim

        -- Recalculate prunable torrents for missing files
        tid <- readTVar . torrentToIDs $ m
        let p = IS.fromList . IM.keys . IM.filter (\fs -> null (fs `HS.intersection` ais)) $ tid
        writeTVar (prunable m) p
  | otherwise = pure ()

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
