{-# LANGUAGE GADTs #-}
module UI.Types (View(..),
                KeyEvent(..),
                AppState(AppState),
                Events(..),
                view,
                torrents,
                session,
                sessionStats,
                keyBindings,
                keyHandler,
                queue,
                visibleMenu,
                menuCursor,
                mainCursor,
                sortKey,
                reverseSort,
                selected,
                Menu(..),
                newState
  )
where
import           Brick                           (EventM)
import           Brick.Keybindings               (KeyConfig)
import           Brick.Keybindings.KeyDispatcher (KeyDispatcher)
import           Data.IntSet                     (IntSet)
import           Effectful.Concurrent.STM        (TVar)
import           Transmission.RPC.Session        (Session, SessionStats,
                                                  emptySession,
                                                  emptySessionStats)
import           Transmission.RPC.Torrent        (Torrent)
import           Types                           (FIFOSet, Sort (Name), Req)

data View = Main | Downloading | Seeding | Complete | Paused | Inactive | Error | Prune deriving (Eq, Ord, Show)

data KeyEvent = QuitEvent
              | MainViewEvent
              | DownloadingViewEvent
              | SeedingViewEvent
              | CompleteViewEvent
              | PausedViewEvent
              | InactiveViewEvent
              | ErrorViewEvent
              | PruneViewEvent
              | SortMenuEvent
              | CloseMenuEvent
              | CursorDownEvent
              | CursorUpEvent
              | CursorTriggerEvent
              | PageUpEvent
              | PageDownEvent
              | SelectEvent
              | SelectAllEvent
              | SelectNoneEvent
              | SelectUpEvent
              | SelectDownEvent
              | RemoveSelectedEvent
              | RemoveSelectedWithDataEvent
            deriving (Eq, Ord)

data AppState where
  AppState :: {
               view :: View,
               torrents :: [Torrent],
               session :: Session,
               sessionStats :: SessionStats,
               keyBindings :: KeyConfig KeyEvent,
               keyHandler :: KeyDispatcher KeyEvent (EventM String AppState),
               queue :: TVar (FIFOSet (Bool,View, Req)),
               visibleMenu :: Menu,
               menuCursor :: Int,
               mainCursor :: Int,
               sortKey :: Sort,
               reverseSort :: Bool,
               selected :: IntSet
               } ->
                AppState

data Events where
  Tick :: Events
  Updated :: Bool -> View -> [Torrent] -> Session -> SessionStats -> Events

data Menu = NoMenu | Sort deriving Eq


newState ::  KeyConfig KeyEvent -> KeyDispatcher KeyEvent (EventM String AppState) -> TVar (FIFOSet (Bool, View, Req)) -> AppState
newState keyConfig dispatcher q = AppState Main [] emptySession emptySessionStats keyConfig dispatcher q NoMenu 0 0 Name False mempty
