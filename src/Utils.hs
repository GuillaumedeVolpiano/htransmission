{-# LANGUAGE FlexibleContexts #-}

module Utils
  ( mkPathMap
  , sortTorrents
  , buildFilesPath
  , getFileNodes
  )
where
import           Data.Function                (on, (&))
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HM (insert)
import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as HS (insert)
import           Data.List                    (sortBy, isPrefixOf)
import           Data.Maybe                   (fromJust)
import qualified Streamly.Data.Fold           as F (foldl')
import           Streamly.Data.Fold           (Fold)
import qualified Streamly.Data.Stream         as S (fold, fromList, unfoldrM, catMaybes)
import qualified Streamly.Data.Stream.Prelude as S (parConcatMap)
import           Streamly.Data.Stream.Prelude (Stream, eager)
import           System.Directory             (listDirectory)
import           System.FilePath.Posix        ((</>), normalise, splitDirectories, joinPath)
import           System.Posix                 (deviceID, fileID, getFileStatus,
                                               isDirectory, FileStatus)
import           Transmission.RPC.Torrent     (Torrent, addedDate, downloadDir,
                                               downloadedEver, eta, fName,
                                               files, labels, name,
                                               peersConnected, percentComplete, rateDownload,
                                               rateUpload, ratio, toId,
                                               totalSize, uploadedEver,
                                               webseeds)
import           Types                        (PathMap, Sort (..), UFID (UFID))
import Control.Exception (IOException, try)


mkPathMap :: [(FilePath, FilePath)] -> FilePath -> FilePath
mkPathMap pm fp = foldr remapIfMatch fp pm
  where
    remapIfMatch (from, to) path
      | isRoot from path = replaceRoot from to path
      | otherwise = path

isRoot :: FilePath -> FilePath -> Bool
isRoot root fp = (splitDirectories . normalise $ root) `isPrefixOf` (splitDirectories . normalise $ fp)

replaceRoot :: FilePath -> FilePath -> FilePath -> FilePath
replaceRoot from to toMod = normalise . joinPath $ splitNormTo ++ drop (length splitNormFrom) splitNormMod
  where
    splitNormFrom = splitDirectories . normalise $ from
    splitNormTo = splitDirectories . normalise $ to
    splitNormMod = splitDirectories . normalise $ toMod
 
buildFilesPath :: PathMap -> Torrent -> [FilePath]
buildFilesPath pathMap torrent = map (dir </>) fns
  where
    fns = map fName . fromJust . files $ torrent
    dir = pathMap . fromJust . downloadDir $ torrent

sortTorrents :: Sort -> Bool -> [Torrent] -> [Torrent]
sortTorrents sk reversed = rev . sortBy (key <> (compare `on` (fromJust . toId)))
  where
    rev = if reversed  then reverse else id
    key = case sk of
                Name            -> compare `on` name
                PercentComplete -> compare `on` percentComplete
                Downloaded      -> compare `on` downloadedEver
                DownloadSpeed   -> compare `on` rateDownload
                Uploaded        -> compare `on` uploadedEver
                UploadSpeed     -> compare `on` rateUpload
                ETA             -> compare `on` eta
                Ratio           -> compare `on` ratio
                TotalSize       -> compare `on` totalSize
                Peers           -> compare `on` peersConnected
                Seeds           -> compare `on` length . webseeds
                DateAdded       -> compare `on` addedDate
                Labels          -> compare `on` labels

getFileNodes :: [FilePath] -> IO (HashSet UFID, HashMap FilePath UFID)
getFileNodes arrs = S.fromList arrs & S.parConcatMap (eager True) getAllPairs & S.fold foldFileNodesPairs

getFileNodePair :: FilePath -> IO (Maybe (FilePath, UFID))
getFileNodePair fp = do
      es <- try (getFileStatus fp) :: IO (Either IOException FileStatus)
      case es of
        Left _ -> pure Nothing
        Right s -> do
          let i = fileID s
              d = deviceID s
          pure . Just $ (fp, UFID (i, d))

foldFileNodesPairs :: Monad m => Fold m (FilePath, UFID) (HashSet UFID, HashMap FilePath UFID)
foldFileNodesPairs = F.foldl' (\(fis, fpm) (fp, fid) -> (HS.insert fid fis, HM.insert fp fid fpm)) mempty

getAllPairs :: FilePath -> Stream IO (FilePath, UFID)
getAllPairs fp = S.unfoldrM getCurPairs [fp] & S.catMaybes 
  where
    getCurPairs [] = pure Nothing
    getCurPairs (f:fps) = do
      es <- try (getFileStatus f) :: IO (Either IOException FileStatus)
      case es of
        Left _ -> getCurPairs fps
        Right s -> do
          if isDirectory s then do
                              names <- listDirectory f
                              let children = map (f </>) names
                              getCurPairs (children ++ fps)
                            else do
                              pair <- getFileNodePair f
                              pure . Just $ (pair, fps)
