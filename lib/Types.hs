{-# LANGUAGE GADTs #-}
module Types
  (View(..),
  AppState(AppState),
  PathMap,
  Events(..),
  KeyEvent(..),
  view,
  client,
  torrents,
  session,
  sessionStats,
  log,
  keyBindings,
  keyHandler
  )
where
import Prelude hiding (log)
import Transmission.RPC.Client (Client)
import Transmission.RPC.Torrent (Torrent)
import Transmission.RPC.Session (Session, SessionStats)
import Data.Text (Text)
import Brick.Keybindings (KeyConfig, KeyDispatcher)
import Brick (EventM)

data View = Main | Downloading | Seeding | Complete | Paused | Inactive | Error | Prune deriving (Eq, Show)

data KeyEvent = QuitEvent
              | MainViewEvent
              | DownloadingViewEvent
              | SeedingViewEvent
              | CompleteViewEvent
              | PausedViewEvent
              | InactiveViewEvent
              | ErrorViewEvent
              | PruneViewEvent
            deriving (Eq, Ord)

type PathMap = FilePath -> FilePath

data AppState where
  AppState :: {
               view :: View,
               client :: Client,
               torrents :: [Torrent],
               session :: Session,
               sessionStats :: SessionStats,
               log :: [Text],
               keyBindings :: KeyConfig KeyEvent,
               keyHandler :: KeyDispatcher KeyEvent (EventM Int AppState)
               } ->
                AppState

data Events = Tick
