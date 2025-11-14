{-# LANGUAGE GADTs #-}
module Types
  (
  PathMap,
  Action(..),
  Sort(..),
  Req(..),
  UpdateEvent(..),
  View(..),
  KeyEvent(..),
  AppState(AppState),
  Events(..),
  view,
  torrents,
  session,
  sessionStats,
  keyBindings,
  keyHandler,
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
  request,
  Menu(..),
  newState,
  DialogContent (..),
  getView,
  ClientState,
  curView,
  clientState,
  newClientState
  )
where
import           Transmission.RPC.Types (Label)
import           Brick                           (EventM)
import           Brick.Keybindings               (KeyConfig)
import           Brick.Keybindings.KeyDispatcher (KeyDispatcher)
import           Brick.Widgets.Dialog            (Dialog)
import           Data.IntSet                     (IntSet)
import           Data.Text
import           Effectful.Concurrent.STM        (TVar, TChan)
import           Transmission.RPC.Session        (Session, SessionStats,
                                                  emptySession,
                                                  emptySessionStats)
import           Transmission.RPC.Torrent        (Torrent)

data Action = Global | Matched deriving (Eq, Ord)

type PathMap = FilePath -> FilePath

data Sort = Name | PercentComplete | Downloaded | DownloadSpeed | Uploaded | UploadSpeed | ETA | Ratio | TotalSize
  | Peers | Seeds | DateAdded | Labels deriving (Enum, Eq, Ord)

data UpdateEvent where
  ReqEvent :: Req -> UpdateEvent
  TimerMinor :: UpdateEvent
  TimerMajor :: UpdateEvent
  deriving (Show, Eq)

data Req = Get | Delete ([Int], Bool) | Add [(FilePath, FilePath, [Label])] deriving (Eq, Ord, Show)

data View = Main | Downloading | Seeding | Complete | Paused | Inactive | Error | Unmatched
          | SingleTorrent Int View Int | Active
  deriving (Eq, Ord, Show)

data KeyEvent =
                ActiveViewEvent
              | CloseMenuEvent
              | CompleteViewEvent
              | CursorDownEvent
              | CursorUpEvent
              | CursorLeftEvent
              | CursorRightEvent
              | CursorTriggerEvent
              | DownloadingViewEvent
              | ErrorViewEvent
              | InactiveViewEvent
              | MainViewEvent
              | MenuOffEvent
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
               keyBindings :: KeyConfig KeyEvent,
               keyHandler :: KeyDispatcher KeyEvent (EventM String AppState),
               visibleMenu :: Menu,
               menuCursor :: Int,
               mainCursor :: Int,
               selected :: IntSet,
               mainOffset :: Int,
               mainVisibleHeight :: Int,
               visibleWidth :: Int,
               mainContentHeight :: Int,
               visibleDialog :: Maybe (Dialog (Maybe DialogContent) String),
               clientState :: TVar ClientState,
               request :: TChan Req
               } ->
                AppState

data ClientState where
  ClientState :: {
                  curView :: View,
                  sortKey :: Sort,
                  reverseSort :: Bool
                 } -> ClientState

data Events where
  Updated :: [Torrent] -> Session -> SessionStats -> Events

data Menu = NoMenu | Sort | Single deriving Eq

data DialogContent = Alert Text | Remove ([Int], Bool)

newState ::  KeyConfig KeyEvent -> KeyDispatcher KeyEvent (EventM String AppState)
         -> TVar ClientState -> TChan Req -> AppState
newState keyConfig dispatcher = AppState Main [] emptySession emptySessionStats keyConfig dispatcher
  NoMenu 0 0 mempty 0 0 0 0 Nothing

getView :: View -> View
getView (SingleTorrent _ v _) = v
getView v = v

newClientState :: ClientState
newClientState = ClientState Main Name False
