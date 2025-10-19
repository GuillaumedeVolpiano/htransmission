{-# LANGUAGE GADTs #-}
module Types
  (View(..),
  AppState(AppState),
  PathMap,
  view,
  torrents,
  session,
  seshStats)
where
import Transmission.RPC.Torrent (Torrent)
import Transmission.RPC.Session (Session, SessionStats)

data View = Main | Prune deriving (Eq, Show)

type PathMap = FilePath -> FilePath

data AppState where
  AppState :: {view :: View,
               torrents :: [Torrent],
               session :: Session,
               seshStats :: SessionStats} ->
                AppState
