{-# LANGUAGE GADTs #-}
module Types
  (
  PathMap,
  Action(..),
  Sort(..),
  Req(..)
  )
where
import           Transmission.RPC.Types (Label)

data Action = Global | Matched deriving (Eq, Ord)

type PathMap = FilePath -> FilePath

data Sort = Name | PercentComplete | Downloaded | DownloadSpeed | Uploaded | UploadSpeed | ETA | Ratio | TotalSize
  | Peers | Seeds | DateAdded | Labels deriving (Enum, Eq, Ord)

data Req = Get | Delete ([Int], Bool) | Add [(FilePath, FilePath, [Label])] deriving (Eq, Ord)
