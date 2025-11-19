{-# LANGUAGE OverloadedStrings #-}
module UI.KeyEvents (
                      keyConfig,
                      dispatcher
                    )
where
import           Brick                           (EventM, halt)
import           Brick.Keybindings               (Binding,
                                                  EventTrigger (ByEvent, ByKey),
                                                  KeyConfig, KeyEventHandler,
                                                  KeyEvents, bind, ctrl,
                                                  handlerDescription,
                                                  kehEventTrigger, kehHandler,
                                                  keyDispatcher, keyEventName,
                                                  khHandler, meta, newKeyConfig,
                                                  onEvent, shift)
import           Brick.Keybindings.KeyDispatcher (KeyDispatcher)
import           Brick.Keybindings.KeyEvents     (keyEvents)
import           Brick.Keybindings.Pretty        (ppBinding)
import           Data.Maybe                      (fromJust)
import qualified Data.Text                       as T (unpack)
import           Graphics.Vty                    (Key (KChar, KDown, KEnter, KEsc, KFun, KLeft, KPageDown, KPageUp, KRight, KUp))
import           Types                           (AppState, KeyEvent (..),
                                                  Menu (Sort), View (..))
import           UI.Events                       (cursorDown, cursorLeft,
                                                  cursorRight, cursorTrigger,
                                                  cursorUp, menuOff, menuOnOff,
                                                  pageDown, pageUp,
                                                  removeTorrent, reverseSort,
                                                  selectAll, selectDown,
                                                  selectNone, selectOne,
                                                  selectUp, switchView,
                                                  tabSwitch)

allKeyEvents :: KeyEvents KeyEvent
allKeyEvents = keyEvents [
                          ("quit", QuitEvent),
                          ("main view", MainViewEvent),
                          ("downloading view", DownloadingViewEvent),
                          ("seeding view", SeedingViewEvent),
                          ("complete view", CompleteViewEvent),
                          ("paused view", PausedViewEvent),
                          ("inactive view", InactiveViewEvent),
                          ("error view", ErrorViewEvent),
                          ("unreferenced view", UnmatchedViewEvent),
                          ("close menu or single torrent view", MenuOffEvent),
                          ("display sort menu", SortMenuEvent),
                          ("reverse sort order", ReverseSortEvent),
                          ("close menu", CloseMenuEvent),
                          ("move cursor down", CursorDownEvent),
                          ("move cursor up", CursorUpEvent),
                          ("move cursor right", CursorRightEvent),
                          ("move cursor left", CursorLeftEvent),
                          ("cursor trigger", CursorTriggerEvent),
                          ("move the main view one page down", PageDownEvent),
                          ("move the main view one page up", PageUpEvent),
                          ("select one item", SelectEvent),
                          ("select all", SelectAllEvent),
                          ("deselect all", SelectNoneEvent),
                          ("select multiple, upwards", SelectUpEvent),
                          ("select multiple, downwards", SelectDownEvent),
                          ("remove the selected torrents", RemoveSelectedEvent),
                          ("remove the selected torrents and their data", RemoveSelectedWithDataEvent),
                          ("switch the focused button", TabSwitchEvent),
                          ("active view", ActiveViewEvent),
                          ("show the log", LogViewEvent)
                         ]

defaultBindings :: [(KeyEvent, [Binding])]
defaultBindings = [
                   (QuitEvent, [bind (KChar 'q')]),
                   (MainViewEvent, [bind (KChar 'm')]),
                   (DownloadingViewEvent, [meta 'd']),
                   (SeedingViewEvent, [ctrl 's']),
                   (CompleteViewEvent, [ctrl 'c']),
                   (PausedViewEvent, [ctrl 'p']),
                   (InactiveViewEvent, [meta 'i']),
                   (ErrorViewEvent, [ctrl 'e']),
                   (UnmatchedViewEvent, [ctrl 'u']),
                   (MenuOffEvent, [bind KEsc]),
                   (SortMenuEvent, [bind (KFun 7)]),
                   (ReverseSortEvent, [bind (KChar 'r')]),
                   (CursorDownEvent, [bind KDown]),
                   (CursorUpEvent, [bind KUp]),
                   (CursorRightEvent, [bind KRight]),
                   (CursorLeftEvent, [bind KLeft]),
                   (CursorTriggerEvent, [bind KEnter]),
                   (PageDownEvent, [bind KPageDown]),
                   (PageUpEvent, [bind KPageUp]),
                   (SelectEvent, [bind (KChar ' ')]),
                   (SelectAllEvent, [ctrl 'a']),
                   (SelectNoneEvent, [ctrl 'd']),
                   (SelectUpEvent, [shift KUp]),
                   (SelectDownEvent, [shift KDown]),
                   (RemoveSelectedEvent, [bind (KChar '-')]),
                   (RemoveSelectedWithDataEvent, [meta '-']),
                   (TabSwitchEvent, [bind (KChar '\t')]),
                   (ActiveViewEvent, [meta 'a']),
                   (LogViewEvent, [ctrl 'l'])
                  ]

keyConfig :: KeyConfig KeyEvent
keyConfig = newKeyConfig allKeyEvents defaultBindings []

handlers :: [KeyEventHandler KeyEvent (EventM String AppState)]
handlers = [
            onEvent QuitEvent "Quit the program" halt,
            onEvent MainViewEvent "Switch to the main view" (switchView Main),
            onEvent DownloadingViewEvent "Switch to the downloads view" (switchView Downloading),
            onEvent SeedingViewEvent "Switch to the seed view" (switchView Seeding),
            onEvent CompleteViewEvent "Switch to the complete view" (switchView Complete),
            onEvent PausedViewEvent "Switch to the paused view" (switchView Paused),
            onEvent InactiveViewEvent "Switch to the inactive view" (switchView Inactive),
            onEvent ErrorViewEvent "Switch to the error view" (switchView Error),
            onEvent UnmatchedViewEvent "Switch to the unreferenced view" (switchView Unmatched),
            onEvent MenuOffEvent "Exit the current menu or single torrent view" menuOff,
            onEvent SortMenuEvent "Display the sort menu" (menuOnOff Sort),
            onEvent ReverseSortEvent "Reverse the sort order" reverseSort,
            onEvent CursorDownEvent "Move the cursor down" cursorDown,
            onEvent CursorUpEvent "Move the cursor up" cursorUp,
            onEvent CursorRightEvent "Move the cursor right" cursorRight,
            onEvent CursorLeftEvent "Move the cursor left" cursorLeft,
            onEvent CursorTriggerEvent "Trigger the cursor action" cursorTrigger,
            onEvent PageDownEvent "Move the main viewport one page down" pageDown,
            onEvent PageUpEvent "Move the main viewport one page up" pageUp,
            onEvent SelectEvent "Toggle the selection of the cursor item" selectOne,
            onEvent SelectAllEvent "Select everything" selectAll,
            onEvent SelectNoneEvent "Deselect everything" selectNone,
            onEvent SelectUpEvent "Toggle multiple selections, upwards" selectUp,
            onEvent SelectDownEvent "Toggle multiple selections, downwards" selectDown,
            onEvent RemoveSelectedEvent "Remove selected torrents" (removeTorrent False),
            onEvent RemoveSelectedWithDataEvent "Remove selected torrents and their data" (removeTorrent True),
            onEvent TabSwitchEvent "Switch selected button" tabSwitch,
            onEvent ActiveViewEvent "Switch to the active view" (switchView Active),
            onEvent LogViewEvent "Switch to the log view" (switchView Log)
           ]

dispatcher :: KeyDispatcher KeyEvent (EventM String AppState)
dispatcher = case keyDispatcher keyConfig handlers of
               Right d -> d
               Left collisions -> do
                  error ("Error: some key events have the same keys bound to them.\n" ++
                    concatMap (\(b, hs) -> "Handlers with the '" <> T.unpack (ppBinding b) <> "' binding:\n"
                        <> concatMap (\h -> " " <> desc h <> " (" <> trigger h <> ")\n") hs) collisions)
  where
    trigger h = case kehEventTrigger $ khHandler h of
               ByKey k -> "triggered by the key '" <> T.unpack (ppBinding k) <> "'"
               ByEvent e -> "triggered by the event '" <> T.unpack (fromJust . keyEventName allKeyEvents $ e) <> "'"
    desc h = T.unpack . handlerDescription . kehHandler $ khHandler h
