{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Types
  (
  PathMap,
  Action(..),
  Sort(..),
  UpdateEvent(..),
  RPCRequest(..),
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
  viewL,
  torrents,
  torrentsL,
  session,
  sessionL,
  sessionStats,
  sessionStatsL,
  keyBindings,
  keyBindingsL,
  keyHandler,
  keyHandlerL,
  visibleMenu,
  visibleMenuL,
  menuCursor,
  menuCursorL,
  mainCursor,
  mainCursorL,
  sortKey,
  reverseSort,
  selected,
  selectedL,
  mainOffset,
  mainOffsetL,
  mainVisibleHeight,
  mainVisibleHeightL,
  visibleWidth,
  visibleWidthL,
  mainContentHeight,
  mainContentHeightL,
  visibleDialog,
  visibleDialogL,
  request,
  requestL,
  clientLog,
  clientLogL,
  fileBrowser,
  fileBrowserL,
  unmatched,
  unmatchedL,
  visibleTorrents,
  visibleTorrentsL,
  addForm,
  addFormL,
  newTorrentsPaths,
  newTorrentsPathsL,
  Menu(..),
  newState,
  DialogContent (..),
  getView,
  ClientState,
  curView,
  clientState,
  clientStateL,
  newClientState,
  AddTorrent(AddTorrent),
  tags,
  tagsL,
  destination,
  destinationL,
  completed,
  completedL,
  startTorrent,
  startTorrentL,
  )
where
import           Brick                            (EventM)
import           Brick.Forms                      (Form)
import           Brick.Keybindings                (KeyConfig)
import           Brick.Keybindings.KeyDispatcher  (KeyDispatcher)
import           Brick.Types                      (suffixLenses)
import           Brick.Widgets.Dialog             (Dialog)
import           Brick.Widgets.FileBrowser        (FileBrowser)
import           Control.Concurrent.STM           (TBQueue, TMVar)
import           Data.Hashable                    (Hashable, hashWithSalt)
import           Data.HashMap.Strict              (HashMap)
import           Data.HashSet                     (HashSet)
import           Data.IntMap                      (IntMap)
import           Data.IntSet                      (IntSet)
import           Data.Text
import           Effectful.Concurrent.STM         (TChan, TVar)
import qualified Streamly.Internal.FS.Event.Linux as FS (Event)
import           System.Posix                     (CDev (CDev), CIno (CIno),
                                                   DeviceID, FileID)
import           Transmission.RPC.Session         (Session, SessionStats,
                                                   emptySession,
                                                   emptySessionStats)
import           Transmission.RPC.Torrent         (Torrent)
import           Transmission.RPC.Types           (Label)

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
  Add :: [FilePath] -> FilePath -> [Label] -> Bool -> Bool -> RPCPayload
  TimerMajor :: RPCPayload
  TimerMinor :: RPCPayload
  deriving (Show)

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
              , eventQueue :: TBQueue UpdateEvent
              } -> Matcher

data View = Main | Downloading | Seeding | Complete | Paused | Inactive | Error | Unmatched | FileBrowser View | NewTorrentForm View
          | SingleTorrent Int View Int | Active | Log
  deriving (Eq, Ord, Show)

data KeyEvent =
                ActiveViewEvent
              | AddTorrentEvent
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
              | LogViewEvent
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
              | FileBrowserViewEvent
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
               clientLog :: [Text],
               unmatched :: IntSet,
               visibleTorrents :: [Torrent],
               newTorrentsPaths :: [FilePath],
               addForm :: Form AddTorrent Events String,
               clientState :: TVar ClientState,
               request :: TChan RPCPayload,
               fileBrowser :: FileBrowser String
               } ->
                AppState

data ClientState where
  ClientState :: {
                  curView :: View,
                  sortKey :: Sort,
                  reverseSort :: Bool
                 } -> ClientState

data Events where
  Updated :: [Torrent] -> [Torrent] -> Session -> SessionStats -> IntSet -> Events
  LogEvent :: Text -> Events

data Menu = NoMenu | Sort | Single deriving Eq

data DialogContent = Alert Text | Remove ([Int], Bool)

data AddTorrent where
  AddTorrent :: {
                tags :: Text
                , destination :: Text
                , completed :: Bool
                , startTorrent :: Bool
                } -> AddTorrent deriving Show

newtype UFID = UFID {unUFID :: (FileID, DeviceID)} deriving Eq

suffixLenses ''AppState

suffixLenses ''AddTorrent

instance Hashable UFID where
  hashWithSalt s (UFID (CIno w, CDev w')) = hashWithSalt s (w, w')

newState ::  KeyConfig KeyEvent -> KeyDispatcher KeyEvent (EventM String AppState) -> Form AddTorrent Events String
         -> TVar ClientState -> TChan RPCPayload -> FileBrowser String -> AppState
newState keyConfig dispatcher = AppState Main [] emptySession emptySessionStats keyConfig dispatcher
  NoMenu 0 0 mempty 0 0 0 0 Nothing [] mempty [] []

getView :: View -> View
getView (SingleTorrent _ v _) = v
getView v                     = v

newClientState :: ClientState
newClientState = ClientState Main Name False
