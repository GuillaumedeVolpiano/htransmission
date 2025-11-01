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
                visibleWidth,
                mainContentHeight,
                visibleDialog,
                Menu(..),
                newState,
                DialogContent (..),
                Request
  )
where
import           Brick                           (EventM)
import           Brick.Keybindings               (KeyConfig)
import           Brick.Keybindings.KeyDispatcher (KeyDispatcher)
import           Brick.Widgets.Dialog            (Dialog)
import           Data.IntSet                     (IntSet)
import           Data.Text
import           Effectful.Concurrent.STM        (TVar)
import           Transmission.RPC.Session        (Session, SessionStats,
                                                  emptySession,
                                                  emptySessionStats)
import           Transmission.RPC.Torrent        (Torrent)
import           Types                           (FIFOSet, Req, Sort (Name))

data View = Main | Downloading | Seeding | Complete | Paused | Inactive | Error | Unmatched | SingleTorrent Int
  deriving (Eq, Ord, Show)

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
              | UnmatchedViewEvent
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
              | TabSwitchEvent
            deriving (Eq, Ord)

data AppState where
  AppState :: {
               view :: View,
               torrents :: [Torrent],
               session :: Session,
               sessionStats :: SessionStats,
               keyBindings :: KeyConfig KeyEvent,
               keyHandler :: KeyDispatcher KeyEvent (EventM String AppState),
               queue :: TVar Request,
               visibleMenu :: Menu,
               menuCursor :: Int,
               mainCursor :: Int,
               sortKey :: Sort,
               reverseSort :: Bool,
               selected :: IntSet,
               mainOffset :: Int,
               mainVisibleHeight :: Int,
               visibleWidth :: Int,
               mainContentHeight :: Int,
               visibleDialog :: Maybe (Dialog (Maybe DialogContent) String)
               } ->
                AppState

data Events where
  Tick :: Bool -> Events
  Updated :: Bool -> View -> [Torrent] -> Session -> SessionStats -> Events

data Menu = NoMenu | Sort deriving Eq

data DialogContent = Alert Text | Remove ([Int], Bool)

type Request = FIFOSet (Bool, View, Req, Sort, Bool, Bool)

newState ::  KeyConfig KeyEvent -> KeyDispatcher KeyEvent (EventM String AppState) 
         -> TVar (FIFOSet (Bool, View, Req, Sort, Bool, Bool)) -> AppState
newState keyConfig dispatcher q = AppState Main [] emptySession emptySessionStats keyConfig dispatcher
  q NoMenu 0 0 Name False mempty 0 0 0 0 Nothing
