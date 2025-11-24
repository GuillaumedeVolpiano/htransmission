{-# LANGUAGE OverloadedStrings #-}
module UI.Events (
                   eventHandler
                 , updateView
                 , switchView
                 , menuOnOff
                 , cursorDown
                 , cursorUp
                 , cursorTrigger
                 , pageDown
                 , pageUp
                 , removeTorrent
                 , reverseSort
                 , selectOne
                 , selectAll
                 , selectNone
                 , selectUp
                 , selectDown
                 , appStartEvent
                 , tabSwitch
                 , cursorLeft
                 , cursorRight
                 , menuOff
                 , requestAddTorrent
                 )
where

import           Brick                     (BrickEvent (AppEvent, VtyEvent),
                                            EventM, get, getVtyHandle, gets,
                                            modify, put)
import           Brick.Keybindings         (handleKey)
import           Brick.Widgets.Dialog      (dialogButtons, dialogSelection,
                                            getDialogFocus, setDialogFocus)
import           Brick.Widgets.FileBrowser (fileBrowserIsSearching,
                                            fileBrowserSelection,
                                            fileInfoFilePath,
                                            handleFileBrowserEvent)
import           Control.Concurrent.STM    (modifyTVar', readTVar, readTVarIO,
                                            writeTChan)
import           Control.Monad             (unless, void, when)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.STM         (atomically)
import           Data.IntSet               (delete, insert, member)
import qualified Data.IntSet               as IS (fromList)
import qualified Data.IntSet               as S (fromList, toList)
import           Data.Maybe                (fromJust, isNothing)
import           Graphics.Vty              (Event (EvKey, EvResize),
                                            Key (KEnter, KEsc), displayBounds,
                                            outputIface)
import           Transmission.RPC.Torrent  (toId)
import qualified Types                     as T (clientLog, clientState,
                                                 curView, keyHandler, request,
                                                 reverseSort, sortKey, torrents,
                                                 unmatched, view,
                                                 visibleTorrents)
import           Types                     (AppState (visibleDialog),
                                            DialogContent (Alert, Remove),
                                            Events (..),
                                            Menu (NoMenu, Single, Sort),
                                            RPCPayload (Add, Delete, Get),
                                            View (FileBrowser, Log, NewTorrentForm, SingleTorrent, Unmatched),
                                            addForm, addFormL, clientLogL,
                                            completed, destination, fileBrowser,
                                            fileBrowserL, mainCursor,
                                            mainCursorL, mainOffset,
                                            mainOffsetL, mainVisibleHeight,
                                            mainVisibleHeightL, menuCursor,
                                            menuCursorL, newTorrentsPaths,
                                            newTorrentsPathsL, selected,
                                            selectedL, sessionL, sessionStatsL,
                                            startTorrent, tags, torrentsL,
                                            unmatchedL, viewL, visibleDialogL,
                                            visibleMenu, visibleMenuL,
                                            visibleTorrentsL, visibleWidth,
                                            visibleWidthL)
import           UI.Utils                  (mkDialog)
import           Utils                     (sel, sortTorrents)

import           Brick.Forms               (formState, handleFormEvent)
import           Brick.Types               (zoom)
import           Control.Lens              (set)
import           Data.Text                 (Text)
import qualified Data.Text                 as T (splitOn, strip, unpack)
import           Transmission.RPC.Types    (Label)

appStartEvent :: EventM String AppState ()
appStartEvent = do
  vty <- getVtyHandle
  (width, height) <- liftIO $ displayBounds . outputIface $ vty
  modify (\s -> s{mainVisibleHeight = height - 6, visibleWidth = width})
  updateView

eventHandler :: BrickEvent String Events -> EventM String AppState ()
eventHandler ev = do
  view <- gets T.view
  case view of
    NewTorrentForm v -> addFormEventHandler v ev
    _ -> case ev of
                AppEvent (Updated torrents visibleTorrents sesh seshStats unmatched) ->
                  modify (set torrentsL torrents . set sessionL sesh . set sessionStatsL seshStats . set unmatchedL unmatched
                  . set visibleTorrentsL visibleTorrents)
                AppEvent (LogEvent logMessage) -> appLogMessage logMessage
                VtyEvent vev -> do
                  case view of
                    FileBrowser v -> fileBrowserEventHandler v vev
                    _ -> do
                      case vev of
                        EvKey k mods -> gets T.keyHandler >>= \kh -> void (handleKey kh k mods)
                        EvResize newWidth newHeight -> do
                          modify (\s -> set visibleWidthL newWidth . set mainVisibleHeightL (newHeight - 6)
                            . set mainCursorL (min (mainCursor s) (newHeight -1)) $ s)
                        _ -> pure ()
                _ -> pure ()


appLogMessage :: Text -> EventM String AppState ()
appLogMessage logMessage = modify (\a -> set clientLogL (logMessage : T.clientLog a) a)

addFormEventHandler :: View -> BrickEvent String Events -> EventM String AppState ()
addFormEventHandler view ev = do
  case ev of
    VtyEvent (EvKey KEsc []) -> modify (set viewL view . set newTorrentsPathsL [])
    VtyEvent (EvKey KEnter []) -> addTorrent
    _ -> zoom addFormL $ handleFormEvent ev


fileBrowserEventHandler :: View -> Event -> EventM String AppState ()
fileBrowserEventHandler view ev = do
            fb <- gets fileBrowser
            case ev of
              EvKey KEsc []
                | not (fileBrowserIsSearching fb) -> modify (set viewL view)
              _ -> do
                  _ <- zoom fileBrowserL $ handleFileBrowserEvent ev
                  case ev of
                    EvKey KEnter [] -> cursorTriggerFileBrowser
                    _               -> pure ()

updateView :: EventM n AppState ()
updateView = do
  request <- gets T.request
  liftIO . atomically $ writeTChan request (Get Nothing)

switchView :: View -> EventM n AppState ()
switchView view = do
  csVar <- gets T.clientState
  unmatched <- gets T.unmatched
  unless (view == Unmatched && isNothing unmatched) $ do
    (rs, sortKey) <- liftIO . atomically $ do
      cs <- readTVar csVar
      let r = T.reverseSort cs
          s = T.sortKey cs
      modifyTVar' csVar (\st -> st{T.curView = view})
      pure (r, s)
    torrents <- gets T.torrents
    let t' = sel view unmatched sortKey rs torrents
    modify (set viewL view. set mainOffsetL 0 . set mainCursorL 0 . set visibleTorrentsL t')

menuOnOff :: Menu -> EventM n AppState ()
menuOnOff menu = do
  curMenu <- gets visibleMenu
  let menu' = if curMenu == menu then NoMenu else menu
  modify (set visibleMenuL menu' . set menuCursorL 0)

menuOff :: EventM n AppState ()
menuOff = do
  curView <- gets T.view
  curMenu <- gets visibleMenu
  case curView of
    SingleTorrent _ v _ -> modify (set visibleMenuL NoMenu . set viewL v . set menuCursorL 0)
    _ -> unless (curMenu == NoMenu) $ modify (set visibleMenuL NoMenu . set menuCursorL 0)

cursorDown :: EventM n AppState ()
cursorDown = do
  vm <- gets visibleMenu
  if vm  == NoMenu then cursorDownMain else cursorDownMenu

cursorLeft :: EventM n AppState ()
cursorLeft = do
  vd <- gets visibleDialog
  case vd of
    Just d -> do
      let buttons = dialogButtons d
          focus = getDialogFocus d
          i = maybe 0 read focus
          i' = (i - 1) `mod` length buttons
          d' = setDialogFocus (show i') d
      modify (set visibleDialogL (Just d'))
    Nothing -> do
      view <- gets T.view
      case view of
        SingleTorrent idx v pos -> unless (idx == 0) $ do
          modify (set viewL (SingleTorrent (idx - 1) v pos))
        _ -> pure ()

cursorRight :: EventM n AppState ()
cursorRight = do
  vd <- gets visibleDialog
  case vd of
    Just d -> do
      let buttons = dialogButtons d
          focus = getDialogFocus d
          i = maybe 0 read focus
          i' = (i + 1) `mod` length buttons
          d' = setDialogFocus (show i') d
      modify (set visibleDialogL (Just d'))
    Nothing -> do
      view <- gets T.view
      maxIdx <- (-1 +) . length <$> gets T.visibleTorrents
      case view of
        SingleTorrent idx v pos -> unless (idx == maxIdx) $ do
          modify (set viewL (SingleTorrent (idx + 1) v pos))
        _ -> pure ()

cursorDownMain :: EventM n AppState ()
cursorDownMain = do
  s <- get
  let cursor = mainCursor s
      tors = T.visibleTorrents s
      offset = mainOffset s
      height = mainVisibleHeight s
      cursor' = if cursor == height - 1 || cursor + offset == length tors - 1 then cursor else cursor + 1
      offset' = if cursor == height - 1 then offset + 1 else offset
  put s {mainCursor = cursor', mainOffset = offset'}

cursorDownMenu :: EventM n AppState ()
cursorDownMenu = do
  menu <- gets visibleMenu
  cursor <- gets menuCursor
  let menuSize = case menu of
                   Sort   -> 12
                   Single -> 4
                   NoMenu -> error "Trying to set menu size when there is no menu displayed"
      cursor' = min (cursor + 1) menuSize
  modify (set menuCursorL cursor')

cursorUp :: EventM n AppState ()
cursorUp = do
  vm <- gets visibleMenu
  if vm == NoMenu then cursorUpMain else cursorUpMenu

cursorUpMain :: EventM n AppState ()
cursorUpMain = do
  cursor <- gets mainCursor
  offset <- gets mainOffset
  let cursor' = if cursor == 0 then cursor else cursor -1
      offset' = if cursor == 0 then max (offset - 1) 0 else offset
  modify (set mainCursorL cursor' . set mainOffsetL offset')

cursorUpMenu :: EventM n AppState ()
cursorUpMenu = do
  cursor <- gets menuCursor
  let cursor' = max (cursor - 1) 0
  modify (set menuCursorL cursor')

cursorTrigger :: EventM n AppState ()
cursorTrigger = do
  maybeDialog <- gets visibleDialog
  case maybeDialog of
    Nothing -> cursorTriggerNoDialog
    Just aDialog -> do
      let maybeResponse = dialogSelection aDialog
      case maybeResponse of
          Nothing -> pure ()
          Just (_, response) -> case response of
             Nothing -> pure ()
             Just (Alert _) -> pure ()
             Just (Remove (toRemove, removeData)) -> do
               request <- gets T.request
               liftIO . atomically $ writeTChan request (Delete (IS.fromList toRemove, removeData))
               modify (set selectedL mempty)
      modify (set visibleDialogL Nothing)

cursorTriggerNoDialog :: EventM n AppState ()
cursorTriggerNoDialog = do
  view <- gets T.view
  case view of
    Log -> pure ()
    _ -> do
      vm <- gets visibleMenu
      case vm of
        NoMenu -> viewTorrent
        Sort   -> setSortKey
        Single -> do
          p <- gets menuCursor
          v <- gets T.view
          case v of
            SingleTorrent idx v' _ -> modify (set viewL (SingleTorrent idx v' p))
            _ -> undefined -- Single menu should only be visible in SingleTorrentView

cursorTriggerFileBrowser :: EventM n AppState ()
cursorTriggerFileBrowser = do
  files <- map fileInfoFilePath . fileBrowserSelection <$> gets fileBrowser
  unless (null files) $ addTorrentForm files

pageDown :: EventM String AppState ()
pageDown = do
  offset <- gets mainOffset
  vSize <- gets mainVisibleHeight
  maxVal <- (-1 +) . length <$> gets T.visibleTorrents
  let offset' = min (offset + vSize) maxVal
  modify (set mainCursorL 0 . set mainOffsetL offset')

pageUp :: EventM String AppState ()
pageUp = do
  offset <- gets mainOffset
  vSize <- gets mainVisibleHeight
  let offset' = max (offset - vSize) 0
  modify (set mainCursorL 0 . set mainOffsetL offset')

viewTorrent :: EventM n AppState ()
viewTorrent = do
  cursor <- gets mainCursor
  offset <- gets mainOffset
  curView <- gets T.view
  let pos = cursor + offset
  modify (set visibleMenuL Single)
  switchView (SingleTorrent pos curView 0)


setSortKey :: EventM n AppState ()
setSortKey = do
  sk <- toEnum <$> gets menuCursor
  csVar <- gets T.clientState
  clientState <- liftIO . readTVarIO $ csVar
  let curSk = T.sortKey clientState
      reversed = T.reverseSort clientState
  let reversed' = if sk == curSk then not reversed else reversed
  torrents' <- sortTorrents curSk reversed' <$> gets T.visibleTorrents
  liftIO  . atomically . modifyTVar' csVar $ (\s -> s{T.sortKey = sk, T.reverseSort = reversed'})
  modify (set visibleMenuL NoMenu . set visibleTorrentsL torrents' . set menuCursorL 0)

reverseSort :: EventM n AppState ()
reverseSort = do
  csVar <- gets T.clientState
  clientState <- liftIO . readTVarIO $ csVar
  let reversed = T.reverseSort clientState
      sk = T.sortKey clientState
  torrents' <- sortTorrents sk (not reversed) <$> gets T.visibleTorrents
  liftIO . atomically . modifyTVar' csVar $ (\s -> s{T.reverseSort = not reversed})
  modify (set visibleTorrentsL torrents')


selectOne :: EventM n AppState ()
selectOne = do
  vm <- gets visibleMenu
  when (vm == NoMenu) $ do
    selection <- gets selected
    cursor <- gets mainCursor
    offset <- gets mainOffset
    toAdd <- fromJust . toId . (!! (cursor + offset)) <$> gets T.visibleTorrents
    let selection' = if toAdd `member` selection then delete toAdd selection
                                                 else insert toAdd selection
    modify (set selectedL selection')

selectAll :: EventM n AppState ()
selectAll = do
  vm <- gets visibleMenu
  when (vm == NoMenu) $ do
    allIds <- S.fromList . map (fromJust . toId) <$> gets T.visibleTorrents
    modify (set selectedL allIds)

selectNone :: EventM n AppState ()
selectNone = do
  vm <- gets visibleMenu
  when (vm == NoMenu) $ do
    modify (set selectedL mempty)

selectUp :: EventM n AppState ()
selectUp = do
  vm  <- gets visibleMenu
  when (vm == NoMenu) $ selectOne >> cursorUp

selectDown :: EventM n AppState ()
selectDown = do
  vm <- gets visibleMenu
  when (vm == NoMenu) $ selectOne >> cursorDown

removeTorrent :: Bool -> EventM n AppState ()
removeTorrent removeData = do
  width <- gets visibleWidth
  toRemove <- S.toList <$> gets selected
  let theDialog = if null toRemove then Alert "No torrent selected"
                                   else Remove (toRemove, removeData)
  modify (set visibleDialogL (Just (mkDialog theDialog width)))

tabSwitch :: EventM n AppState ()
tabSwitch = do
  vd <- gets visibleDialog
  case vd of
    Nothing -> pure ()
    Just d -> do
      let buttons = dialogButtons d
          focus = getDialogFocus d
          i = maybe 0 read focus
          i' = (i + 1) `mod` length buttons
          d' = setDialogFocus (show i') d
      modify (set visibleDialogL (Just d'))

requestAddTorrent :: EventM n AppState ()
requestAddTorrent = do
    view <- gets T.view
    case view of
      FileBrowser _    -> pure ()
      NewTorrentForm _ -> pure ()
      _                -> modify (set viewL (FileBrowser view))

addTorrentForm :: [FilePath] -> EventM n AppState ()
addTorrentForm fps = do
  view <- gets T.view
  let view' = case view of
                FileBrowser v -> v
                _             -> undefined
  modify (set viewL (NewTorrentForm view') . set newTorrentsPathsL fps)

addTorrent :: EventM n AppState ()
addTorrent = do
  fps <- gets newTorrentsPaths
  s <- formState <$> gets addForm
  view <- gets T.view
  r <- gets T.request
  let labels = map T.strip . T.splitOn "," . tags $ s :: [Label]
      downloadDir = T.unpack . destination $ s
      comp = completed s
      st = startTorrent s
      view' = case view of
                NewTorrentForm v -> v
                _                -> undefined
  liftIO . atomically $ writeTChan r (Add fps downloadDir labels comp st)
  modify (set viewL view' . set newTorrentsPathsL [])
