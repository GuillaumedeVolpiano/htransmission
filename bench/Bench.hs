{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Constants                    (arrPaths, basicSession, labels,
                                               mainTorrents, pathMap)
import           Control.Concurrent.Async     (concurrently, race_,
                                               replicateConcurrently_)
import           Control.Concurrent.STM       (TBQueue, TVar, atomically,
                                               isEmptyTBQueue, newTBQueue,
                                               newTBQueueIO, newTMVarIO,
                                               newTVarIO, putTMVar, readTBQueue,
                                               readTVar, readTVarIO,
                                               tryReadTBQueue, writeTBQueue,
                                               writeTVar)
import           Control.DeepSeq              (NFData (rnf), force)
import           Control.Exception            (IOException, evaluate, try, bracket)
import           Control.Monad                (forever, unless, void)
import           Data.Function                ((&))
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HM (insert, insertWith, union)
import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as HS (insert, union)
import qualified Data.IntMap                  as IM (insert, member)
import           Data.IntMap.Strict           (IntMap)
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IS (insert, singleton, toList,
                                                     union)
import           Data.List                    (intersect)
import           Data.Maybe                   (fromJust)
import           Data.Text                    (Text)
import           Effectful                    (Eff, runEff, (:>))
import           Effectful.Log                (Log, LogLevel (LogTrace), runLog)
import           Effectful.Prim               (Prim, runPrim)
import           Effectful.Time               (Time, runTime)
import           Effectful.Wreq               (Wreq, runWreq)
import           GHC.Generics                 (Generic)
import           Log.Backend.StandardOutput   (withStdOutLogger)
import qualified Streamly.Data.Fold           as F (foldl', foldlM')
import           Streamly.Data.Fold           (Fold)
import qualified Streamly.Data.Fold.Prelude   as F (parBuffered)
import qualified Streamly.Data.Stream         as S (filterM, fold, fromList, mapM)
import           Streamly.Data.Stream         (Stream)
import qualified Streamly.Data.Stream.Prelude as S (parConcatMap, catMaybes, unfoldrM, parMapM, parConcat)
import           Streamly.Data.Stream.Prelude (eager, parList)
import           Streamly.Matcher             (updatePrunable)
import           System.Directory             (listDirectory)
import           System.Posix                 (FileStatus, getFileStatus,
                                               isDirectory, fileID, deviceID)
import           System.Posix.Types           (CDev (CDev), CIno (CIno))
import           Test.Tasty.Bench             (bench, bgroup, defaultMain, env,
                                               nfIO)
import           Transmission.RPC.Client      (Client, getTorrents,
                                               )
import Effectful.RPC.Client (runClient)
import Effectful.Network.HTTP.Client (runHttpClient)
import qualified Transmission.RPC.Torrent     as T (downloadDir, labels)
import           Transmission.RPC.Torrent     (Torrent, downloadDir, fName,
                                               files, progress, toId)
import           Transmission.RPC.Types       (ID (ID), IDs (IDs))
import           Types                        (PathMap, UFID (UFID))
import           Utils                        (mkPathMap, mkUfid)
import Control.Monad.IO.Class (MonadIO)
import Data.Sequence (Seq(Empty, (:<|)))
import qualified Data.Sequence as Sq (singleton, fromList)
import System.Posix.ByteString.FilePath (RawFilePath)
import qualified System.Posix.Files.ByteString as B (getFileStatus)
import qualified System.Posix.ByteString as B (isDirectory)
import System.Posix.Directory.ByteString (openDirStream, closeDirStream, readDirStream)
import qualified Data.ByteString as BS (null, append)
import qualified Data.ByteString.Char8 as BS (pack, last, unsnoc, span, break, all, uncons, unpack)
import Data.ByteString (ByteString)
import qualified Data.List as L (uncons)
import Streamly.FileSystem.DirIO (readEither)
import qualified Streamly.Internal.FileSystem.PosixPath as S (unsafeFromString, toString)
import qualified Streamly.FileSystem.Path as S (Path)
import Streamly.Internal.FileSystem.DirIO (readEitherPaths)
import Streamly.Internal.FileSystem.PosixPath (PosixPath(PosixPath))
import qualified Streamly.Data.Array as A (fromList, toList)
import qualified Data.ByteString as BSR (pack, unpack)

data Response = T Int [FilePath] Rational [Text] FilePath | End

instance NFData Response where
  rnf (T i fps p ls dd) = rnf i `seq` rnf fps `seq` rnf p `seq` rnf ls `seq` rnf dd
  rnf End = rnf ()

data Matcher where
  Matcher ::  {
                maxThreads :: Int
              , arrs :: [FilePath]
              , arrIDs :: TVar (HashSet UFID)
              , arrIDsMap :: TVar (HashMap FilePath UFID)
              , idToTorrents :: TVar (HashMap UFID IntSet)
              , torrentToIDs :: TVar (IntMap (HashSet UFID))
              , prunable :: TVar IntSet
              , eventQueue :: TBQueue Response
              } -> Matcher

splitDirectories :: RawFilePath -> [RawFilePath]
splitDirectories = map dropTrailingPathSeparator . splitPath
  where
    dropTrailingPathSeparator f = case BS.unsnoc f of
                                     Nothing       -> ""
                                     Just (i, '/') -> i
                                     _             -> f

splitPath :: RawFilePath -> [RawFilePath]
splitPath p = [drive | not (BS.null drive)] ++ f path
  where
    (drive, path) = BS.span (=='/') p
    f "" = []
    f y = (a `BS.append` c) : f d
      where
        (a, b) = BS.break (=='/') y
        (c, d) = BS.span (=='/') b

normalise :: RawFilePath -> RawFilePath
normalise path = addPathSeparator result
  where
    (drv, pth) = BS.span (=='/') path
    result = joinDrive' (normaliseDrive drv) (f pth)
    addPathSeparator p
      | hasTrailingPathSeparator pth && not (hasTrailingPathSeparator result) = p `BS.append` "/"
      | otherwise = p
    joinDrive' a b
      | BS.null a && BS.null b = "."
      | BS.null a = b
      | BS.null b = a
      | hasTrailingPathSeparator a = a `BS.append` b
      | otherwise = a `BS.append` "/" `BS.append` b
    f = joinPath . filter (/=".") . propSep . splitDirectories
    propSep xs = case L.uncons xs of
                   Just (x, xs') -> if BS.all (=='/') x then "/" : xs' else xs
                   Nothing       -> []

combine :: RawFilePath -> RawFilePath -> RawFilePath
combine a b
  | hasLeadingPathSeparator b = b
  | otherwise = combineAlways a b

combineAlways :: RawFilePath -> RawFilePath -> RawFilePath
combineAlways a b
  | BS.null a = b
  | BS.null b = a
  | hasTrailingPathSeparator a = a `BS.append` b
  | otherwise = a <> "/" <> b

hasLeadingPathSeparator :: RawFilePath -> Bool
hasLeadingPathSeparator fp = (fst <$> BS.uncons fp) == Just '/'

normaliseDrive :: RawFilePath -> RawFilePath
normaliseDrive "" = ""
normaliseDrive _  = "/"

joinPath :: [RawFilePath] -> RawFilePath
joinPath = foldr combine ""

hasTrailingPathSeparator :: RawFilePath -> Bool
hasTrailingPathSeparator p = case BS.unsnoc p of
                               Just (_, '/') -> True
                               _             -> False

(</>) :: RawFilePath -> RawFilePath -> RawFilePath
(</>) = combine

-- getElements :: (Client :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es) =>
--     Maybe IntSet -> Eff es [Torrent]
-- getElements torrents = do
--     let tids = IDs . map ID . IS.toList <$> torrents
--     getTorrents tids (Just mainTorrents) Nothing
-- 
-- matchWorker :: Matcher -> Stream IO Response -> IO IntSet
-- matchWorker mt stream = do
--   race_ (spawnWorkers mt) (matcher mt [stream])
--   readTVarIO . prunable $ mt
-- 
-- matchPar :: Matcher -> Stream IO Response -> IO IntSet
-- matchPar mt stream = do
--   stream & S.filterM (needed mt) & S.fold (consumer mt)
--   readTVarIO . prunable $ mt
-- 
-- consumer :: Matcher -> Fold IO Response ()
-- consumer mt = F.parBuffered id . F.foldlM' consume $ pure ()
--   where
--     consume _ ev = case ev of
--                     t@T{} -> updateNewTorrents t mt
--                     _     -> pure ()
-- 
-- needed :: Matcher -> Response -> IO Bool
-- needed mt r = case r of
--                 t@(T i f p l _) -> atomically $ do
--                   tid <- readTVar . torrentToIDs $ mt
--                   -- Only update if torrent is complete, has relevant labels, and is not already in the mapping
--                   pure . not $ (i `IM.member` tid || p /= 100 || null (l `intersect` labels))
--                 _ -> pure True
-- 
-- spawnWorkers :: Matcher -> IO ()
-- spawnWorkers mt = replicateConcurrently_ (maxThreads mt) (worker mt)
-- 
-- matcher :: Matcher-> [Stream IO Response] -> IO ()
-- matcher mt streams = oneStream & S.filterM (needed mt) & S.fold (consumer mt)
--   where
--     oneStream = parList (eager True) streams
-- 
-- worker :: Matcher -> IO ()
-- worker mt = do
--   ev <- atomically . tryReadTBQueue $ eventQueue mt
--   case ev of
--     Just End -> do
--       empty <- atomically . isEmptyTBQueue $ eventQueue mt
--       if empty then match mt End
--                else do
--                  atomically . writeTBQueue (eventQueue mt) $ End
--                  worker mt
--     Just t -> match mt t >> worker mt
--     Nothing -> worker mt
-- 
-- match :: Matcher -> Response -> IO ()
-- match m u = case u of
--                   t@T{} -> updateNewTorrents t m
--                   End -> do
--                     void . readTVarIO . prunable $ m
-- 
buildFilesPath :: PathMap -> Response -> [RawFilePath]
buildFilesPath pathMap (T _ fp _ _ dd) = map ((dir </>) . BS.pack) fp
  where
    dir = pathMap . BS.pack $ dd

updateTorrentMaps :: Int -> HashSet UFID -> IntMap (HashSet UFID) -> HashMap UFID IntSet -> (IntMap (HashSet UFID), HashMap UFID IntSet)
updateTorrentMaps ti nodelist tid itd = (updateIm, updateHm)
  where
    updateIm = IM.insert ti nodelist tid
    updateHm = foldr (\fid -> HM.insertWith IS.union fid (IS.singleton ti)) itd nodelist

-- updateNewTorrents ::  Response -> Matcher -> IO ()
-- updateNewTorrents t@(T i f _ _ _) m = do
--   -- Get the list of filesystem nodes associated with this torrent
--   nodelist <- fmap fst . getFileNodes . buildFilesPath (mkPathMap pathMap) $ t
-- 
--   atomically $ do
--     -- Read current torrent-to-files and file-to-torrent mappings
--     tid <- readTVar . torrentToIDs $ m
--     itd <- readTVar . idToTorrents $ m
-- 
--     -- Update the mappings with the new torrent
--     let (tid', itd') = updateTorrentMaps i nodelist tid itd
--     writeTVar (torrentToIDs m) tid'
--     writeTVar (idToTorrents m) itd'
-- 
--     -- Read current set of known files and prunable torrents
--     ais <- readTVar . arrIDs $ m
--     p <- readTVar . prunable $ m
-- 
--     -- Update prunable set based on new torrent’s files
--     writeTVar (prunable m) $ updatePrunable i tid' ais p

extractTorrent :: Torrent -> Response
extractTorrent t = T (fromJust . toId $ t) (map fName . fromJust . files $ t) (fromJust . progress $ t) (fromJust . T.labels $ t) (fromJust . T.downloadDir $ t)

-- getFileNodes :: [FilePath] -> IO (HashSet UFID, HashMap FilePath UFID)
-- getFileNodes arrs = S.fromList arrs & S.parConcatMap (eager True) getAllPairs & S.fold foldFileNodesPairs

getFileNodesOpt :: [RawFilePath] -> IO (HashSet UFID, HashMap RawFilePath UFID)
getFileNodesOpt arrs = S.fromList arrs & S.parMapM (eager True) getAllPairsOpt & S.parConcat (eager True) & S.fold foldFileNodesPairsOpt

getFileNodesStreamly :: [RawFilePath] -> IO (HashSet UFID, HashMap RawFilePath UFID)
getFileNodesStreamly arrs = S.fromList arrs & S.mapM (getFileNodesOneStreamly . PosixPath . A.fromList . BSR.unpack) & S.fold (F.parBuffered id $ F.foldl' (\(a, b) (c, d) -> (a `HS.union` c, b `HM.union` d)) (mempty, mempty))

getFileNodesOneStreamly :: S.Path -> IO (HashSet UFID, HashMap RawFilePath UFID)
getFileNodesOneStreamly path = readEitherPaths id path & S.parMapM (eager True) fileNodeStream & S.fold foldNodeStream

fileNodeStream :: Either S.Path S.Path -> IO (Either (HashSet UFID, HashMap RawFilePath UFID) (RawFilePath, UFID))
fileNodeStream (Right (PosixPath file)) = do
  let fp = BSR.pack . A.toList $ file
  status <- B.getFileStatus fp
  pure $ Right (fp, mkUfid (fileID status) (deviceID status)) 
fileNodeStream (Left directory) = Left <$> getFileNodesOneStreamly directory

foldNodeStream :: Fold IO (Either (HashSet UFID, HashMap RawFilePath UFID) (RawFilePath, UFID)) (HashSet UFID, HashMap RawFilePath UFID)
foldNodeStream = F.parBuffered id $ F.foldl' foldNodes (mempty, mempty)
  where
    foldNodes (a, b) (Right (c, d)) = (HS.insert d a, HM.insert c d b)
    foldNodes (a, b) (Left (c, d)) = (HS.union a c, HM.union b d)

foldFileNodesPairs :: Fold IO (FilePath, UFID) (HashSet UFID, HashMap FilePath UFID)
foldFileNodesPairs = F.parBuffered id $ F.foldl' (\(fis, fpm) (fp, fid) -> (HS.insert fid fis, HM.insert fp fid fpm)) mempty

foldFileNodesPairsOpt :: Fold IO (RawFilePath, UFID) (HashSet UFID, HashMap RawFilePath UFID)
foldFileNodesPairsOpt = F.parBuffered id $ F.foldl' (\(fis, fpm) (fp, fid) -> (HS.insert fid fis, HM.insert fp fid fpm)) mempty

-- getAllPairs :: FilePath -> Stream IO (FilePath, UFID)
-- getAllPairs fp = S.unfoldrM getCurPairs [fp] & S.catMaybes
--   where
--     getCurPairs [] = pure Nothing
--     getCurPairs (f:fps) = do
--       es <- try (getFileStatus f) :: IO (Either IOException FileStatus)
--       case es of
--         Left _ -> getCurPairs fps
--         Right s -> do
--           if isDirectory s then do
--                               names <- listDirectory f
--                               let children = map (f </>) names
--                               getCurPairs (children ++ fps)
--                             else do
--                               pair <- getFileNodePair f s
--                               pure . Just $ (Just pair, fps)

getAllPairsOpt :: RawFilePath -> IO (Stream IO (RawFilePath, UFID))
getAllPairsOpt fp = pure $ S.unfoldrM getCurPairs (Sq.singleton fp) & S.catMaybes
  where
    getFiles ds = do
      e <- readDirStream ds
      if BS.null e then pure []
                   else (e:) <$> getFiles ds
    a <//> b
      | BS.null a = b
      | BS.last a == '/' = a <> b
      | otherwise = a <> "/" <> b
    getCurPairs Empty = pure Nothing
    getCurPairs (f :<| fps) = do
      es <- try (B.getFileStatus f) :: IO (Either IOException FileStatus)
      case es of
        Left _ -> getCurPairs fps
        Right s -> do
          if B.isDirectory s  then do
                                names <- bracket (openDirStream f) closeDirStream getFiles
                                let children = fmap (f <//>) . Sq.fromList . filter (`notElem` ([".", ".."] :: [ByteString])) $ names
                                getCurPairs (children <> fps)
                              else do
                                pure . Just $ (Just (f, mkUfid (fileID s) (deviceID s)), fps)

getFileNodePairOpt :: RawFilePath -> FileStatus -> IO (RawFilePath, UFID)
getFileNodePairOpt fp s = do
  let i = fileID s
      d = deviceID s
  pure (fp, mkUfid i d)

getFileNodePair :: FilePath -> FileStatus -> IO (FilePath, UFID)
getFileNodePair fp s = do
  let i = fileID s
      d = deviceID s
  pure (fp, mkUfid i d)

main :: IO ()
main = do
  defaultMain [bgroup "Matcher" [ 
                                  bench "UnfoldOpt" . nfIO $ getFileNodesOpt arrPaths 
                                , bench "UnfoldStreamly" . nfIO $ getFileNodesStreamly arrPaths]
              ]

-- main :: IO ()
-- main = do
--   ip <- init <$> readFile "ip.txt"
--   client <- runEff . runWreq . runPrim $ fromUrl ip Nothing Nothing
--   torrents <- withStdOutLogger $ \logger -> runEff . runClient client . runWreq . runPrim . runLog "Client" logger LogTrace . runTime $ getElements Nothing
--   let torrents' = map extractTorrent torrents ++ [End]
--       streamTemplate = S.fromList torrents'
--   defaultMain [env (do
--     fn <- getFileNodes arrPaths
--     (ais, aim) <- evaluate (force fn)
--     return (ais, aim)) $ \ ~(ais, aim) ->
--       bgroup "Matcher" [
--           bench "parBuffered" . nfIO $ do
--             ais' <- newTVarIO ais
--             aim' <- newTVarIO aim
--             itd <- newTVarIO mempty
--             tid <- newTVarIO mempty
--             prunable <- newTVarIO mempty
--             eventQueue <- newTBQueueIO 120
--             let matcher = Matcher 12 arrPaths ais' aim' itd tid prunable eventQueue
--             matchPar matcher streamTemplate
--         , bench "parWorker" . nfIO $ do
--             ais' <- newTVarIO ais
--             aim' <- newTVarIO aim
--             itd <- newTVarIO mempty
--             tid <- newTVarIO mempty
--             prunable <- newTVarIO mempty
--             eventQueue <- newTBQueueIO 120
--             let matcher = Matcher 12 arrPaths ais' aim' itd tid prunable eventQueue
--             matchWorker matcher streamTemplate
--             ]
--               ]
