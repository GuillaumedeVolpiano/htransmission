{-# LANGUAGE GADTs #-}
module Types
  (View(..),
  AppState(AppState),
  PathMap,
  Events(..),
  view,
  client,
  torrents,
  session,
  sessionStats,
  log
  )
where
import Prelude hiding (log)
import Transmission.RPC.Client (Client)
import Transmission.RPC.Torrent (Torrent)
import Transmission.RPC.Session (Session, SessionStats)
import Data.Text (Text)

data View = Main | Prune deriving (Eq, Show)

type PathMap = FilePath -> FilePath

data AppState where
  AppState :: {
               view :: View,
               client :: Client,
               torrents :: [Torrent],
               session :: Session,
               sessionStats :: SessionStats,
               log :: [Text]
               } ->
                AppState

data Events = Tick
