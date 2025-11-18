{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Types
  (
  PathMap,
  Action(..),
  Sort(..),
  UpdateEvent(..),
  RPCRequest(..),
  UpdateConfig(..),
  View(..),
  KeyEvent(..),
  AppState(AppState),
  UIBUS(..),
  Events(..),
  Matcher(..),
  RPCPayload (..),
  Response(..),
  UFID(..),
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
import           Brick                            (EventM)
import           Brick.Keybindings                (KeyConfig)
import           Brick.Keybindings.KeyDispatcher  (KeyDispatcher)
import           Brick.Widgets.Dialog             (Dialog)
import           Data.Hashable                    (Hashable, hashWithSalt)
import           Data.HashMap.Strict              (HashMap)
import           Data.HashSet                     (HashSet)
import           Data.IntMap                      (IntMap)
import           Data.IntSet                      (IntSet)
import           Data.IORef                       (IORef)
import           Data.Text
import           Effectful.Concurrent.STM         (TChan, TVar)
import qualified Streamly.Internal.FS.Event.Linux as FS (Event)
import           System.Posix                     (CIno (CIno), FileID, DeviceID, CDev (CDev))
import           Transmission.RPC.Session         (Session, SessionStats,
                                                   emptySession,
                                                   emptySessionStats)
import           Transmission.RPC.Torrent         (Torrent)
import           Transmission.RPC.Types           (Label)
import Control.Concurrent.STM (TMVar)

data Action = Global | Matched deriving (Eq, Ord)

type PathMap = FilePath -> FilePath

data Sort = Name | PercentComplete | Downloaded | DownloadSpeed | Uploaded | UploadSpeed | ETA | Ratio | TotalSize
  | Peers | Seeds | DateAdded | Labels deriving (Enum, Eq, Ord)

data UpdateEvent where
  FSEvent :: FS.Event -> UpdateEvent
  RPCEvent :: Response -> UpdateEvent

data Response where
  T :: Torrent -> Response
  End :: TMVar IntSet -> Response

data RPCRequest where
  RPCRequest :: { payload :: RPCPayload, chan :: TMVar ([Torrent], TMVar IntSet) } -> RPCRequest

data RPCPayload where
  Get :: Maybe IntSet -> RPCPayload
  Delete :: (IntSet, Bool) -> RPCPayload
  Add :: [(FilePath, FilePath, [Label])] -> RPCPayload
  TimerMajor :: RPCPayload
  TimerMinor :: RPCPayload
  deriving (Show)

data UpdateConfig where
  UpdateConfig :: {
                    watchDirs :: [FilePath]
                  , fsDebounceMs :: Int
                  , parallelism :: Int
                  , tickInterval :: Double -- in s
                  , tickPeriod :: Double
                  } -> UpdateConfig

data UIBUS where
  UIBUS ::  {
              uiIn :: TVar ClientState
            , uiRequest :: TChan RPCRequest
            } -> UIBUS

data Matcher where
  Matcher ::  {
                maxThreads :: Int
              , arrs :: [FilePath]
              , arrIDs :: TVar (HashSet UFID)
              , arrIDsMap :: TVar (HashMap FilePath UFID)
              , idToTorrents :: TVar (HashMap UFID IntSet)
              , torrentToIDs :: TVar (IntMap (HashSet UFID))
              , prunable :: TVar IntSet
              } -> Matcher

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
               request :: TChan RPCPayload
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

newtype UFID = UFID {unUFID :: (FileID, DeviceID)} deriving Eq

instance Hashable UFID where
  hashWithSalt s (UFID (CIno w, CDev w')) = hashWithSalt s (w, w')

newState ::  KeyConfig KeyEvent -> KeyDispatcher KeyEvent (EventM String AppState)
         -> TVar ClientState -> TChan RPCPayload -> AppState
newState keyConfig dispatcher = AppState Main [] emptySession emptySessionStats keyConfig dispatcher
  NoMenu 0 0 mempty 0 0 0 0 Nothing

getView :: View -> View
getView (SingleTorrent _ v _) = v
getView v                     = v

newClientState :: ClientState
newClientState = ClientState Main Name False
