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
                 )
where

import           Brick                    (BrickEvent (AppEvent, VtyEvent),
                                           EventM, continueWithoutRedraw, get,
                                           getVtyHandle, gets, modify, put)
import           Brick.Keybindings        (handleKey)
import           Brick.Widgets.Dialog     (dialogButtons, dialogSelection,
                                           getDialogFocus, setDialogFocus)
import           Control.Monad            (void, when)
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Data.IntSet              (delete, insert, member)
import qualified Data.IntSet              as S (fromList, toList)
import           Data.Maybe               (fromJust)
import           Effectful                (runEff)
import           Effectful.Client         (enqueueShared)
import           Effectful.Concurrent.STM (atomically, runConcurrent)
import           Graphics.Vty             (Event (EvKey, EvResize),
                                           displayBounds, outputIface)
import           Transmission.RPC.Torrent (toId)
import           Types                    (Req (Delete, Get))
import qualified UI.Types                 as T (keyHandler, queue, reverseSort,
                                                session, sessionStats, sortKey,
                                                torrents, view)
import           UI.Types                 (AppState (visibleDialog),
                                           DialogContent (Alert, Remove),
                                           Events (..), Menu (NoMenu, Sort),
                                           View (SingleTorrent),
                                           mainCursor, mainOffset,
                                           mainVisibleHeight, menuCursor,
                                           selected, visibleMenu, visibleWidth)
import           UI.Utils                 (actionFromView, mkDialog)
import           Utils                    (sortTorrents)

appStartEvent :: EventM String AppState ()
appStartEvent = do
  vty <- getVtyHandle
  (width, height) <- liftIO $ displayBounds . outputIface $ vty
  modify (\s -> s{mainVisibleHeight = height - 6, visibleWidth = width})
  updateView True True

eventHandler :: BrickEvent String Events -> EventM String AppState ()
eventHandler (AppEvent (Tick major)) = updateView False major >> continueWithoutRedraw
eventHandler (AppEvent (Updated isSwitching view torrents sesh seshStats)) =
  if isSwitching then modify (\a -> a{T.view = view, T.torrents = sortTorrents (T.sortKey a) (T.reverseSort a) torrents, T.session = sesh
    , T.sessionStats = seshStats})
  else do
    curView <- gets T.view
    if actionFromView view == actionFromView curView then
                                         modify (\a -> a{T.torrents = sortTorrents (T.sortKey a) (T.reverseSort a) torrents, T.session = sesh
                                           , T.sessionStats = seshStats})
                                         else continueWithoutRedraw
eventHandler (VtyEvent (EvKey k mods)) = gets T.keyHandler >>= \kh -> void (handleKey kh k mods)
eventHandler (VtyEvent (EvResize newWidth newHeight)) = do
  modify (\s -> s{visibleWidth = newWidth, mainVisibleHeight = newHeight - 6
    , mainCursor = min (mainCursor s) (newHeight -1)})
  updateView False False
eventHandler _ = pure ()

updateView :: Bool -> Bool -> EventM n AppState ()
updateView isSwitching isMajor = do
  view <- gets T.view
  fifoVar <- gets T.queue
  sortKey <- gets T.sortKey
  rSort <- gets T.reverseSort
  liftIO $ runEff . runConcurrent . atomically $ do
    enqueueShared (isSwitching, view, Get, sortKey, rSort, isMajor) fifoVar

switchView :: View -> EventM n AppState ()
switchView view = modify (\a -> a{T.view=view}) >> updateView True False >> continueWithoutRedraw

menuOnOff :: Menu -> EventM n AppState ()
menuOnOff menu = do
  curMenu <- gets visibleMenu
  let menu' = if curMenu == menu then NoMenu else menu
  modify (\s -> s{visibleMenu = menu', menuCursor = 0})


cursorDown :: EventM n AppState ()
cursorDown = do
  vm <- gets visibleMenu
  if vm  == NoMenu then cursorDownMain else cursorDownMenu

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
     vm <- gets visibleMenu
     case vm of
       NoMenu -> viewTorrent
       Sort   -> setSortKey
    Just aDialog -> do
      let maybeResponse = dialogSelection aDialog
      case maybeResponse of
          Nothing -> pure ()
          Just (_, response) -> case response of
             Nothing -> pure ()
             Just (Alert _) -> pure ()
             Just (Remove (toRemove, removeData)) -> do
               fifoVar <- gets T.queue
               view <- gets T.view
               sortKey <- gets T.sortKey
               rSort <- gets T.reverseSort
               liftIO $ runEff . runConcurrent . atomically $
                enqueueShared (False, view, Delete (toRemove, removeData), sortKey, rSort, True) fifoVar
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
  switchView (SingleTorrent pos curView)


setSortKey :: EventM n AppState ()
setSortKey = do
  sk <- toEnum <$> gets menuCursor
  curSk <- gets T.sortKey
  reversed <- gets T.reverseSort
  let reversed' = if sk == curSk then not reversed else reversed
  torrents' <- sortTorrents curSk reversed' <$> gets T.torrents
  modify (\s -> s{T.sortKey = sk, visibleMenu = NoMenu, T.reverseSort = reversed', T.torrents = torrents'})

reverseSort :: EventM n AppState ()
reverseSort = do
  reversed <- gets T.reverseSort
  sk <- gets T.sortKey
  torrents' <- sortTorrents sk (not reversed) <$> gets T.torrents
  modify (\s -> s{T.reverseSort = not reversed, T.torrents = torrents'})


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

tabSwitch :: EventM n AppState ()
tabSwitch = do
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
