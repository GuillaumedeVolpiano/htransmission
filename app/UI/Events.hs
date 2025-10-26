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
                 , selectOne
                 , selectAll
                 , selectNone
                 , selectUp
                 , selectDown
                 )
where

import           Brick                    (BrickEvent (AppEvent, VtyEvent),
                                           EventM, get, gets, modify, put)
import           Brick.Keybindings        (handleKey)
import           Brick.Main               (continueWithoutRedraw)
import           Control.Monad            (void, when, unless)
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Data.IntSet              (delete, insert, member)
import           Data.Maybe               (fromJust)
import           Effectful                (runEff)
import           Effectful.Client         (enqueueShared)
import           Effectful.Concurrent.STM (atomically, runConcurrent)
import           Graphics.Vty             (Event (EvKey))
import           Transmission.RPC.Torrent (toId)
import qualified UI.Types                 as T (keyHandler, queue, session,
                                                sessionStats, torrents, view)
import           UI.Types                 (AppState,
                                           Events (..), Menu (NoMenu, Sort),
                                           View, mainCursor, menuCursor,
                                           reverseSort, selected, sortKey, visibleMenu)
import           UI.Utils                 (actionFromView, sel)
import qualified Data.IntSet as S (fromList, toList)
import Types (Req(Get, Delete))

import Utils (sortTorrents)

eventHandler :: BrickEvent String Events -> EventM String AppState ()
eventHandler (AppEvent Tick) = updateView False >> continueWithoutRedraw
eventHandler (AppEvent (Updated isSwitching view torrents sesh seshStats)) =
  if isSwitching then modify (\a -> a{T.view = view, T.torrents = sortTorrents (sortKey a) (reverseSort a) torrents, T.session = sesh
    , T.sessionStats = seshStats})
  else do
    curView <- gets T.view
    if actionFromView view == actionFromView curView then
                                         modify (\a -> a{T.torrents = sortTorrents (sortKey a) (reverseSort a) torrents, T.session = sesh
                                           , T.sessionStats = seshStats})
                                         else continueWithoutRedraw
eventHandler (VtyEvent (EvKey k mods)) = gets T.keyHandler >>= \kh -> void (handleKey kh k mods)
eventHandler _ = pure ()

updateView :: Bool -> EventM n AppState ()
updateView isSwitching = do
  view <- gets T.view
  fifoVar <- gets T.queue
  liftIO $ runEff . runConcurrent . atomically $ do
    enqueueShared (isSwitching, view, Get) fifoVar

switchView :: View -> EventM n AppState ()
switchView view = modify (\a -> a{T.view=view}) >> updateView True >> continueWithoutRedraw

menuOnOff :: Menu -> EventM n AppState ()
menuOnOff menu = do
  curMenu <- gets visibleMenu
  let menu' = if curMenu == menu then NoMenu else menu
  modify (\s -> s{visibleMenu = menu', menuCursor = 0})


cursorDown :: EventM n AppState ()
cursorDown = do
  vm <- gets visibleMenu
  if vm  == NoMenu then cursorDownMain else cursorDownMenu

cursorDownMain :: EventM n AppState ()
cursorDownMain = do
  s <- get
  let cursor = mainCursor s
      tors = sel s
      cursor' = min (cursor + 1) (length tors - 1)
  put s {mainCursor = cursor'}

cursorDownMenu :: EventM n AppState ()
cursorDownMenu = do
  menu <- gets visibleMenu
  cursor <- gets menuCursor
  let menuSize = case menu of
                   Sort   -> 12
                   NoMenu -> undefined
      cursor' = min (cursor + 1) menuSize
  modify (\s -> s {menuCursor = cursor'})

cursorUp :: EventM n AppState ()
cursorUp = do
  vm <- gets visibleMenu
  if vm == NoMenu then cursorUpMain else cursorUpMenu

cursorUpMain :: EventM n AppState ()
cursorUpMain = do
  cursor <- gets mainCursor
  let cursor' = max (cursor - 1) 0
  modify (\s -> s {mainCursor = cursor'})

cursorUpMenu :: EventM n AppState ()
cursorUpMenu = do
  cursor <- gets menuCursor
  let cursor' = max (cursor - 1) 0
  modify (\s -> s{menuCursor = cursor'})

cursorTrigger :: EventM n AppState ()
cursorTrigger = do
  vm <- gets visibleMenu
  case vm of
    NoMenu -> viewTorrent
    Sort   -> setSortKey

pageDown :: EventM String AppState ()
pageDown = undefined

pageUp :: EventM String AppState ()
pageUp = undefined

viewTorrent :: EventM n AppState ()
viewTorrent = undefined -- need to build single torrent view

setSortKey :: EventM n AppState ()
setSortKey = do
  sk <- toEnum <$> gets menuCursor
  curSk <- gets sortKey
  reversed <- gets reverseSort
  let reversed' = if sk == curSk then not reversed else reversed
  torrents' <- sortTorrents curSk reversed' <$> gets T.torrents
  modify (\s -> s{sortKey = sk, visibleMenu = NoMenu, reverseSort = reversed', T.torrents = torrents'})
  pure ()

selectOne :: EventM n AppState ()
selectOne = do
  vm <- gets visibleMenu
  when (vm == NoMenu) $ do
    selection <- gets selected
    cursor <- gets mainCursor
    toAdd <- fromJust . toId . (!! cursor) <$> gets T.torrents
    let selection' = if toAdd `member` selection then delete toAdd selection
                                                 else insert toAdd selection
    modify (\s -> s{selected = selection'})

selectAll :: EventM n AppState ()
selectAll = do
  vm <- gets visibleMenu
  when (vm == NoMenu) $ do
    allIds <- S.fromList . map (fromJust . toId) <$> gets T.torrents
    modify (\s -> s{selected = allIds})

selectNone :: EventM n AppState ()
selectNone = do
  vm <- gets visibleMenu
  when (vm == NoMenu) $ do
    modify (\s -> s{selected = mempty})

selectUp :: EventM n AppState ()
selectUp = do
  vm  <- gets visibleMenu
  when (vm == NoMenu) $ selectOne >> cursorUp

selectDown :: EventM n AppState ()
selectDown = do
  vm <- gets visibleMenu
  when (vm == NoMenu) $ selectOne >> cursorDown

removeTorrent :: Bool -> EventM n AppState ()
removeTorrent removeData = do
  view <- gets T.view
  fifoVar <- gets T.queue
  toRemove <- S.toList <$> gets selected
  unless (null toRemove) $ -- need to report if selection is empty, need failsafe
    liftIO $ runEff . runConcurrent . atomically $ do
      enqueueShared (False, view, Delete (toRemove, removeData)) fifoVar
