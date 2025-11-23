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
import           Data.IntMap                      (IntMap)
import           Data.IntSet                      (IntSet)
import           Data.Text
import           Effectful.Concurrent.STM         (TChan, TVar)
import qualified Streamly.Internal.FS.Event.Linux as FS (Event)
import           Transmission.RPC.Session         (Session, SessionStats,
                                                   emptySession,
                                                   emptySessionStats)
import           Transmission.RPC.Torrent         (Torrent)
import           Transmission.RPC.Types           (Label)
import Streamly.Data.Stream (Stream)
import System.Posix.ByteString (RawFilePath)
import Log.Monad (LogT)
import Control.DeepSeq (NFData (rnf))

data Action = Global | Matched deriving (Eq, Ord)

type PathMap = RawFilePath -> RawFilePath

data Sort = Name | PercentComplete | Downloaded | DownloadSpeed | Uploaded | UploadSpeed | ETA | Ratio | TotalSize
  | Peers | Seeds | DateAdded | Labels deriving (Enum, Eq, Ord)

data UpdateEvent where
  FSEvent :: FS.Event -> UpdateEvent
  RPCEvent :: Stream (LogT IO) Torrent -> TMVar IntSet -> UpdateEvent

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
              arrs :: [RawFilePath]
              , arrIDs :: TVar (IntMap IntSet)
              , arrIDsMap :: TVar (HashMap RawFilePath UFID)
              , idToTorrents :: TVar (IntMap (IntMap IntSet))
              , torrentToIDs :: TVar (IntMap (IntMap IntSet))
              , prunable :: TVar IntSet
              , prunableReady :: TVar Bool
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
               unmatched :: Maybe IntSet,
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
  Updated :: [Torrent] -> [Torrent] -> Session -> SessionStats -> Maybe IntSet -> Events
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

data UFID = UFID {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving Eq -- UFID DevID FileID

suffixLenses ''AppState

suffixLenses ''AddTorrent

instance Hashable UFID where
  hashWithSalt s (UFID w w') = hashWithSalt s (w, w')

instance NFData UFID where
  rnf (UFID d i) = rnf d `seq` rnf i

newState ::  KeyConfig KeyEvent -> KeyDispatcher KeyEvent (EventM String AppState) -> Form AddTorrent Events String
         -> TVar ClientState -> TChan RPCPayload -> FileBrowser String -> AppState
newState keyConfig dispatcher = AppState Main [] emptySession emptySessionStats keyConfig dispatcher
  NoMenu 0 0 mempty 0 0 0 0 Nothing [] mempty [] []

getView :: View -> View
getView (SingleTorrent _ v _) = v
getView v                     = v

newClientState :: ClientState
newClientState = ClientState Main Name False
