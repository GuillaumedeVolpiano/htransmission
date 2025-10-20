{-# LANGUAGE OverloadedStrings #-}
module UI.KeyEvents (
                      keyConfig,
                      dispatcher
                    )
where
import Brick.Keybindings
    ( Binding,
      KeyEvents,
      bind,
      ctrl,
      KeyConfig,
      newKeyConfig,
      KeyEventHandler,
      onEvent,
      EventTrigger(ByKey, ByEvent),
      keyEventName,
      khHandler,
      kehEventTrigger,
      kehHandler,
      handlerDescription,
      keyDispatcher )
import           Brick.Keybindings.KeyEvents (keyEvents)
import           Graphics.Vty                (Key (KEsc))
import Brick (EventM, halt)
import Types (AppState, KeyEvent(..), View (..))
import UI.Events (switchView)
import Brick.Keybindings.KeyDispatcher (KeyDispatcher)
import Brick.Keybindings.Pretty (ppBinding)
import qualified Data.Text as T (unpack)
import Data.Maybe (fromJust)

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
                          ("unreferenced view", PruneViewEvent)
                         ]

defaultBindings :: [(KeyEvent, [Binding])]
defaultBindings = [
                   (QuitEvent, [bind KEsc]),
                   (MainViewEvent, [ctrl 'm']),
                   (DownloadingViewEvent, [ctrl 'd']),
                   (SeedingViewEvent, [ctrl 's']),
                   (CompleteViewEvent, [ctrl 'c']),
                   (PausedViewEvent, [ctrl 'p']),
                   (InactiveViewEvent, [ctrl 'i']),
                   (ErrorViewEvent, [ctrl 'e']),
                   (PruneViewEvent, [ctrl 'u'])
                  ]

keyConfig :: KeyConfig KeyEvent
keyConfig = newKeyConfig allKeyEvents defaultBindings []

handlers :: [KeyEventHandler KeyEvent (EventM n AppState)]
handlers = [
            onEvent QuitEvent "Quit the program" halt,
            onEvent MainViewEvent "Switch to the main view" (switchView Main),
            onEvent DownloadingViewEvent "Switch to the downloads view" (switchView Downloading),
            onEvent SeedingViewEvent "Switch to the seed view" (switchView Seeding),
            onEvent CompleteViewEvent "Switch to the complete view" (switchView Complete),
            onEvent PausedViewEvent "Switch to the paused view" (switchView Paused),
            onEvent InactiveViewEvent "Switch to the inactive view" (switchView Inactive),
            onEvent ErrorViewEvent "Switch to the error view" (switchView Error),
            onEvent PruneViewEvent "Switch to the unreferenced view" (switchView Prune)
           ]

dispatcher :: KeyDispatcher KeyEvent (EventM n AppState)
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
