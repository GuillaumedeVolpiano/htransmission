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
import           Graphics.Vty                    (Key (KChar, KDown, KFun, KUp, KEnter, KPageDown, KPageUp))
import           UI.Events                       (cursorDown, cursorUp,
                                                  menuOnOff, switchView, cursorTrigger,
                                                  pageUp, pageDown, selectOne, selectAll,
                                                  selectNone, selectUp, selectDown, removeTorrent, reverseSort, tabSwitch)
import           UI.Types                        (AppState, KeyEvent (..),
                                                  Menu (Sort), View (..))

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
                          ("unreferenced view", PruneViewEvent),
                          ("display sort menu", SortMenuEvent),
                          ("reverse sort order", ReverseSortEvent),
                          ("close menu", CloseMenuEvent),
                          ("move cursor down", CursorDownEvent),
                          ("move cursor up", CursorUpEvent),
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
                          ("switch the focused button", TabSwitchEvent)
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
                   (PruneViewEvent, [ctrl 'u']),
                   (SortMenuEvent, [bind (KFun 7)]),
                   (ReverseSortEvent, [bind (KChar 'r')]),
                   (CursorDownEvent, [bind KDown]),
                   (CursorUpEvent, [bind KUp]),
                   (CursorTriggerEvent, [bind KEnter]),
                   (PageDownEvent, [bind KPageDown]),
                   (PageUpEvent, [bind KPageUp]),
                   (SelectEvent, [bind (KChar ' ')]),
                   (SelectAllEvent, [ctrl 'a']),
                   (SelectNoneEvent, [ctrl 'd']),
                   (SelectUpEvent, [shift KUp]),
                   (SelectDownEvent, [shift KDown]),
                   (RemoveSelectedEvent, [bind (KChar '-')]), -- switch to '-' and ctrl '-' once confirmation integrated
                   (RemoveSelectedWithDataEvent, [meta '-']),
                   (TabSwitchEvent, [bind (KChar '\t')])
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
            onEvent PruneViewEvent "Switch to the unreferenced view" (switchView Prune),
            onEvent SortMenuEvent "Display the sort menu" (menuOnOff Sort),
            onEvent ReverseSortEvent "Reverse the sort order" reverseSort,
            onEvent CursorDownEvent "Move the cursor down" cursorDown,
            onEvent CursorUpEvent "Move the cursor up" cursorUp,
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
            onEvent TabSwitchEvent "Switch selected button" tabSwitch
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
