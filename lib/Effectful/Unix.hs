{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Effectful.Unix
  (Unix
  , runUnix
  , fileID
  , isDirectory
  , getDirectoryContents
  , listDirectory)
where
import Effectful (Effect, Eff, (:>), IOE, liftIO)
import System.Posix.Types (FileID)
import Effectful.Dispatch.Dynamic (interpret)
import qualified System.Directory as D (getDirectoryContents, listDirectory)
import System.Posix.Files (getFileStatus)
import qualified System.Posix.Files as P (isDirectory, fileID)
import Effectful.TH (makeEffect)

data Unix :: Effect where
  FileID :: FilePath -> Unix m FileID
  IsDirectory :: FilePath -> Unix m Bool
  ListDirectory :: FilePath -> Unix m [FilePath]
  GetDirectoryContents :: FilePath -> Unix m [FilePath]
makeEffect ''Unix

runUnix :: IOE :> es => Eff (Unix : es) a -> Eff es a
runUnix = interpret $ \_ -> \case 
                                FileID path -> liftIO $ P.fileID <$> getFileStatus path
                                IsDirectory path -> liftIO $ P.isDirectory <$> getFileStatus path
                                ListDirectory path -> liftIO $ D.listDirectory path
                                GetDirectoryContents path -> liftIO $ D.getDirectoryContents path
