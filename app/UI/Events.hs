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
                 )
where

import           Brick                    (BrickEvent (AppEvent, VtyEvent),
                                           EventM, get, getVtyHandle, gets,
                                           modify, put)
import           Brick.Keybindings        (handleKey)
import           Brick.Widgets.Dialog     (dialogButtons, dialogSelection,
                                           getDialogFocus, setDialogFocus)
import           Control.Monad            (unless, void, when)
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Data.IntSet              (delete, insert, member)
import qualified Data.IntSet              as IS (fromList)
import qualified Data.IntSet              as S (fromList, toList)
import           Data.Maybe               (fromJust)
import           Effectful                (runEff)
import           Effectful.Concurrent.STM (atomically, modifyTVar', readTVarIO,
                                           runConcurrent, writeTChan)
import           Graphics.Vty             (Event (EvKey, EvResize),
                                           displayBounds, outputIface)
import           Transmission.RPC.Torrent (toId)
import qualified Types                    as T (clientLog, clientState, curView,
                                                keyHandler, request,
                                                reverseSort, session,
                                                sessionStats, sortKey, torrents,
                                                view)
import           Types                    (AppState (visibleDialog),
                                           DialogContent (Alert, Remove),
                                           Events (..),
                                           Menu (NoMenu, Single, Sort),
                                           RPCPayload (Delete, Get),
                                           View (Log, SingleTorrent),
                                           mainCursor, mainOffset,
                                           mainVisibleHeight, menuCursor,
                                           selected, visibleMenu, visibleWidth)
import           UI.Utils                 (mkDialog)
import           Utils                    (sortTorrents)

appStartEvent :: EventM String AppState ()
appStartEvent = do
  vty <- getVtyHandle
  (width, height) <- liftIO $ displayBounds . outputIface $ vty
  modify (\s -> s{mainVisibleHeight = height - 6, visibleWidth = width})
  updateView

eventHandler :: BrickEvent String Events -> EventM String AppState ()
eventHandler (AppEvent (Updated torrents sesh seshStats)) =
  modify (\a -> a{T.torrents = torrents, T.session = sesh, T.sessionStats = seshStats})
eventHandler (AppEvent (LogEvent logMessage)) =
  modify (\a -> a{T.clientLog = logMessage : T.clientLog a})
eventHandler (VtyEvent (EvKey k mods)) = gets T.keyHandler >>= \kh -> void (handleKey kh k mods)
eventHandler (VtyEvent (EvResize newWidth newHeight)) = do
  modify (\s -> s{visibleWidth = newWidth, mainVisibleHeight = newHeight - 6
    , mainCursor = min (mainCursor s) (newHeight -1)})
  updateView
eventHandler _ = pure ()

updateView :: EventM n AppState ()
updateView = do
  request <- gets T.request
  liftIO $ runEff . runConcurrent . atomically $ writeTChan request (Get Nothing)

switchView :: View -> EventM n AppState ()
switchView view = do
  csVar <- gets T.clientState
  liftIO . runEff . runConcurrent . atomically $ modifyTVar' csVar (\s -> s{T.curView = view})
  modify (\a -> a{T.view=view, mainOffset = 0, mainCursor = 0 })
  updateView

menuOnOff :: Menu -> EventM n AppState ()
menuOnOff menu = do
  curMenu <- gets visibleMenu
  let menu' = if curMenu == menu then NoMenu else menu
  modify (\s -> s{visibleMenu = menu', menuCursor = 0})

menuOff :: EventM n AppState ()
menuOff = do
  curView <- gets T.view
  curMenu <- gets visibleMenu
  case curView of
    SingleTorrent _ v _ -> modify (\s -> s{visibleMenu = NoMenu, T.view = v, menuCursor = 0})
    _ -> unless (curMenu == NoMenu) $ modify (\s -> s{visibleMenu = NoMenu, menuCursor = 0})

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
      modify (\s -> s {visibleDialog = Just d'})
    Nothing -> do
      view <- gets T.view
      case view of
        SingleTorrent idx v pos -> unless (idx == 0) $ do
          modify (\s -> s{T.view = SingleTorrent (idx - 1) v pos})
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
      modify (\s -> s {visibleDialog = Just d'})
    Nothing -> do
      view <- gets T.view
      maxIdx <- (-1 +) . length <$> gets T.torrents
      case view of
        SingleTorrent idx v pos -> unless (idx == maxIdx) $ do
          modify (\s -> s{T.view = SingleTorrent (idx + 1) v pos})
        _ -> pure ()

cursorDownMain :: EventM n AppState ()
cursorDownMain = do
  s <- get
  let cursor = mainCursor s
      tors = T.torrents s
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
  modify (\s -> s {menuCursor = cursor'})

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
  modify (\s -> s {mainCursor = cursor', mainOffset = offset'})

cursorUpMenu :: EventM n AppState ()
cursorUpMenu = do
  cursor <- gets menuCursor
  let cursor' = max (cursor - 1) 0
  modify (\s -> s{menuCursor = cursor'})

cursorTrigger :: EventM n AppState ()
cursorTrigger = do
  maybeDialog <- gets visibleDialog
  case maybeDialog of
    Nothing -> do
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
               SingleTorrent idx v' _ -> modify (\s -> s{T.view=SingleTorrent idx v' p})
               _ -> undefined -- Single menu should only be visible in SingleTorrentView

    Just aDialog -> do
      let maybeResponse = dialogSelection aDialog
      case maybeResponse of
          Nothing -> pure ()
          Just (_, response) -> case response of
             Nothing -> pure ()
             Just (Alert _) -> pure ()
             Just (Remove (toRemove, removeData)) -> do
               request <- gets T.request
               liftIO $ runEff . runConcurrent . atomically $ writeTChan request (Delete (IS.fromList toRemove, removeData))
               modify (\s -> s{selected=mempty})
      modify (\s -> s{visibleDialog = Nothing})

pageDown :: EventM String AppState ()
pageDown = do
  offset <- gets mainOffset
  vSize <- gets mainVisibleHeight
  maxVal <- (-1 +) . length <$> gets T.torrents
  let offset' = min (offset + vSize) maxVal
  modify (\s -> s { mainCursor = 0, mainOffset = offset'})

pageUp :: EventM String AppState ()
pageUp = do
  offset <- gets mainOffset
  vSize <- gets mainVisibleHeight
  let offset' = max (offset - vSize) 0
  modify (\s -> s {mainCursor = 0, mainOffset = offset'})

viewTorrent :: EventM n AppState ()
viewTorrent = do
  cursor <- gets mainCursor
  offset <- gets mainOffset
  curView <- gets T.view
  let pos = cursor + offset
  modify (\s -> s{visibleMenu = Single})
  switchView (SingleTorrent pos curView 0)


setSortKey :: EventM n AppState ()
setSortKey = do
  sk <- toEnum <$> gets menuCursor
  csVar <- gets T.clientState
  clientState <- liftIO . runEff . runConcurrent . readTVarIO $ csVar
  let curSk = T.sortKey clientState
      reversed = T.reverseSort clientState
  let reversed' = if sk == curSk then not reversed else reversed
  torrents' <- sortTorrents curSk reversed' <$> gets T.torrents
  liftIO . runEff . runConcurrent . atomically . modifyTVar' csVar $ (\s -> s{T.sortKey = sk, T.reverseSort = reversed'})
  modify (\s -> s{visibleMenu = NoMenu, T.torrents = torrents', menuCursor = 0})

reverseSort :: EventM n AppState ()
reverseSort = do
  csVar <- gets T.clientState
  clientState <- liftIO . runEff . runConcurrent . readTVarIO $ csVar
  let reversed = T.reverseSort clientState
      sk = T.sortKey clientState
  torrents' <- sortTorrents sk (not reversed) <$> gets T.torrents
  liftIO . runEff . runConcurrent . atomically . modifyTVar' csVar $ (\s -> s{T.reverseSort = not reversed})
  modify (\s -> s{T.torrents = torrents'})


selectOne :: EventM n AppState ()
selectOne = do
  vm <- gets visibleMenu
  when (vm == NoMenu) $ do
    selection <- gets selected
    cursor <- gets mainCursor
    offset <- gets mainOffset
    toAdd <- fromJust . toId . (!! (cursor + offset)) <$> gets T.torrents
    let selection' = if toAdd `member` selection then delete toAdd selection
                                                 else insert toAdd selection
    modify (\s -> s{selected = selection'})

selectAll :: EventM n AppState ()
selectAll = do
  vm <- gets visibleMenu
  when (vm == NoMenu) $ do
    allIds <- S.fromList . map (fromJust . toId) <$> gets T.torrents
    modify (\s -> s{selected = allIds})

selectNone :: EventM n AppState ()
selectNone = do
  vm <- gets visibleMenu
  when (vm == NoMenu) $ do
    modify (\s -> s{selected = mempty})

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
  modify (\s -> s {visibleDialog = Just (mkDialog theDialog width)})

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
      modify (\s -> s {visibleDialog = Just d'})
