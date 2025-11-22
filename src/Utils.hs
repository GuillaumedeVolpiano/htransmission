{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( mkPathMap
  , sortTorrents
  , buildFilesPath
  , getFileNodes
  )
where
import           Control.Exception            (IOException, bracket, try)
import qualified Data.ByteString              as BS (append, null)
import qualified Data.ByteString.Char8        as BS (all, break, pack, span,
                                                     uncons, unsnoc)
import           Data.Function                (on, (&))
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HM (insert)
import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as HS (insert)
import           Data.List                    (isPrefixOf, sortBy)
import qualified Data.List                    as L (uncons)
import           Data.Maybe                   (fromJust)
import           Data.Sequence                (Seq (Empty, (:<|)))
import qualified Data.Sequence                as Sq (singleton)
import qualified Streamly.Data.Fold           as F (foldl')
import           Streamly.Data.Fold           (Fold)
import qualified Streamly.Data.Stream         as S (catMaybes, fold, fromList,
                                                    unfoldrM)
import qualified Streamly.Data.Stream.Prelude as S (parConcatMap)
import           Streamly.Data.Stream.Prelude (Stream, eager)
import qualified Streamly.Data.Fold.Prelude as F (parBuffered)
import           System.Posix.ByteString      (FileStatus, RawFilePath,
                                               closeDirStream, deviceID, fileID,
                                               getFileStatus, isDirectory,
                                               openDirStream, readDirStream, DirStream)
import           Transmission.RPC.Torrent     (Torrent, addedDate, downloadDir,
                                               downloadedEver, eta, fName,
                                               files, labels, name,
                                               peersConnected, percentComplete,
                                               rateDownload, rateUpload, ratio,
                                               toId, totalSize, uploadedEver,
                                               webseeds)
import           Types                        (PathMap, Sort (..), UFID (UFID))


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

mkPathMap :: [(RawFilePath, RawFilePath)] -> RawFilePath -> RawFilePath
mkPathMap pm fp = foldr remapIfMatch fp pm
  where
    remapIfMatch (from, to) path
      | isRoot from path = replaceRoot from to path
      | otherwise = path

isRoot :: RawFilePath -> RawFilePath -> Bool
isRoot root fp = (splitDirectories . normalise $ root) `isPrefixOf` (splitDirectories . normalise $ fp)

replaceRoot :: RawFilePath -> RawFilePath -> RawFilePath -> RawFilePath
replaceRoot from to toMod = normalise . joinPath $ splitNormTo ++ drop (length splitNormFrom) splitNormMod
  where
    splitNormFrom = splitDirectories . normalise $ from
    splitNormTo = splitDirectories . normalise $ to
    splitNormMod = splitDirectories . normalise $ toMod

buildFilesPath :: PathMap -> Torrent -> [RawFilePath]
buildFilesPath pathMap torrent = map (dir </>) fns
  where
    fns = map (BS.pack . fName) . fromJust . files $ torrent
    dir = pathMap . BS.pack . fromJust . downloadDir $ torrent

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

getFileNodes :: [RawFilePath] -> IO (HashSet UFID, HashMap RawFilePath UFID)
getFileNodes arrs = S.fromList arrs & S.parConcatMap (eager True) getAllPairs & S.fold foldFileNodesPairs

getFileNodePair :: RawFilePath -> FileStatus -> IO (RawFilePath, UFID)
getFileNodePair fp s = do
          let i = fileID s
              d = deviceID s
          pure (fp, UFID (i, d))

foldFileNodesPairs :: Fold IO (RawFilePath, UFID) (HashSet UFID, HashMap RawFilePath UFID)
foldFileNodesPairs = F.parBuffered id $ F.foldl' (\(fis, fpm) (fp, fid) -> (HS.insert fid fis, HM.insert fp fid fpm)) mempty

readAll :: RawFilePath -> DirStream -> IO (Seq RawFilePath)
readAll f ds = do
  e <- readDirStream ds
  if BS.null e || e == "." || e == ".." then pure Empty
                                        else do
                                          rest <- readAll f ds
                                          pure ((f </> e) :<| rest)

getAllPairs :: RawFilePath -> Stream IO (RawFilePath, UFID)
getAllPairs fp = S.unfoldrM getCurPairs (Sq.singleton fp) & S.catMaybes
  where
    getCurPairs Empty = pure Nothing
    getCurPairs (f :<| fps) = do
      es <- try (getFileStatus f) :: IO (Either IOException FileStatus)
      case es of
        Left _ -> getCurPairs fps
        Right s -> do
          if isDirectory s then do
                              children <- bracket (openDirStream f) closeDirStream (readAll f)
                              getCurPairs (children <> fps)
                            else do
                              pair <- getFileNodePair f s
                              pure . Just $ (Just pair, fps)
