{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module Utils
  (extractPrunable
  , isRoot
  , replaceRoot
  , mkPathMap
  , sortTorrents
  )
where
import qualified Constants                as C (labels)
import           Control.Monad            (forM)
import           Data.Function            (on)
import           Data.List                (intersect, isPrefixOf, sortBy)
import           Data.Maybe               (fromJust, isNothing)
import           Effectful                (Eff, (:>))
import           Effectful.Unix           (Unix, fileID, isDirectory,
                                           listDirectory)
import           System.FilePath.Posix    (joinPath, normalise,
                                           splitDirectories, (</>))
import           System.Posix             (FileID)
import           Transmission.RPC.Torrent (Torrent, addedDate, downloadDir,
                                           downloadedEver, eta, fName, files,
                                           labels, name, peersConnected,
                                           percentComplete, progress,
                                           rateDownload, rateUpload, ratio,
                                           totalSize, uploadedEver, webseeds, toId)
import           Types                    (PathMap, Sort (..))

extractPrunable :: (Unix :> es) => [(FilePath, FilePath)] -> PathMap -> [Torrent] -> Eff es  [Torrent]
extractPrunable arrs pathMap tors = do
  arrIDs <- forM arrs (\(root, full) -> (root, ) <$> (getAllPaths full >>= getFileNodes))
  torIDs <- forM (filter (\t -> ((==100) . fromJust . progress $ t) && (not . null . intersect C.labels . fromJust . labels $ t)) tors) (\t -> (t,) <$> (getFileNodes . buildFilesPath pathMap) t)
  pure $ sortBy (\a b -> compare (name a) (name b)) . map fst . filter (not . referenced pathMap arrIDs) $ torIDs

getFileNodes :: (Unix :> es) => [FilePath] -> Eff es [FileID]
getFileNodes fps = forM fps fileID

getAllPaths :: (Unix :> es) => FilePath -> Eff es [FilePath]
getAllPaths fp = do
  isDir <- isDirectory fp
  if isDir then listDirectory fp >>= (concat <$>) . mapM (getAllPaths . (fp </>))
        else pure [fp]

referenced :: PathMap -> [(FilePath, [FileID])] -> (Torrent, [FileID]) -> Bool
referenced pathMap arrs (tor, inodes) = not . null . intersect inodes . concatMap snd . filter (isRootIf . fst) $ arrs
  where
    maybeFP = downloadDir tor
    isRootIf fp
      | isNothing maybeFP = False
      | otherwise = isRoot fp . pathMap . fromJust $ maybeFP

isRoot :: FilePath -> FilePath -> Bool
isRoot root fp = (splitDirectories . normalise $ root) `isPrefixOf` (splitDirectories . normalise $ fp)

replaceRoot :: FilePath -> FilePath -> FilePath -> FilePath
replaceRoot from to toMod = normalise . joinPath $ splitNormTo ++ drop (length splitNormFrom) splitNormMod
  where
    splitNormFrom = splitDirectories . normalise $ from
    splitNormTo = splitDirectories . normalise $ to
    splitNormMod = splitDirectories . normalise $ toMod

mkPathMap :: [(FilePath, FilePath)] -> FilePath -> FilePath
mkPathMap pm fp = foldr remapIfMatch fp pm
  where
    remapIfMatch (from, to) path
      | isRoot from path = replaceRoot from to path
      | otherwise = path

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
