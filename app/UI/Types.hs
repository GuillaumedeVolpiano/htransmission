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
                mainOffset,
                mainVisibleHeight,
                mainContentHeight,
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

data KeyEvent = 
                CloseMenuEvent
              | CompleteViewEvent
              | CursorDownEvent
              | CursorUpEvent
              | CursorTriggerEvent
              | DownloadingViewEvent
              | ErrorViewEvent
              | InactiveViewEvent
              | MainViewEvent
              | PageUpEvent
              | PageDownEvent
              | PausedViewEvent
              | PruneViewEvent
              | QuitEvent
              | RemoveSelectedEvent
              | RemoveSelectedWithDataEvent
              | ReverseSortEvent
              | SeedingViewEvent
              | SelectEvent
              | SelectAllEvent
              | SelectNoneEvent
              | SelectUpEvent
              | SelectDownEvent
              | SortMenuEvent
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
               selected :: IntSet,
               mainOffset :: Int,
               mainVisibleHeight :: Int,
               mainContentHeight :: Int
               } ->
                AppState

data Events where
  Tick :: Events
  Updated :: Bool -> View -> [Torrent] -> Session -> SessionStats -> Events

data Menu = NoMenu | Sort deriving Eq


newState ::  KeyConfig KeyEvent -> KeyDispatcher KeyEvent (EventM String AppState) -> TVar (FIFOSet (Bool, View, Req)) -> AppState
newState keyConfig dispatcher q = AppState Main [] emptySession emptySessionStats keyConfig dispatcher q NoMenu 0 0 Name False mempty 0 0 0
