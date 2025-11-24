{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Utils
  (
    actionFromView
  , appChooseCursor
  , highlightRow
  , mkDialog
  , withBrickLogger)
where

import           Brick                    (CursorLocation (CursorLocation, cursorLocation, cursorLocationName, cursorLocationVisible),
                                           Location (Location), Widget, str,
                                           txt, withAttr)
import           Brick.BChan              (BChan, writeBChan)
import           Brick.Widgets.Dialog     (Dialog, dialog)
import           Data.List                (find)
import           Data.Maybe               (listToMaybe)
import           Effectful                (MonadUnliftIO, withRunInIO)
import           Log.Internal.Logger      (withLogger, Logger)
import           Types                    (Action (Global, Matched), AppState,
                                           DialogContent (Alert, Remove),
                                           Events (LogEvent), Menu (NoMenu),
                                           View (Unmatched, FileBrowser, NewTorrentForm),
                                           mainCursor, menuCursor, visibleMenu)
import qualified Types as T (view) 
import           UI.Attrs                 (cursorAttr)
import Log (mkLogger, showLogMessage)

actionFromView :: View -> Action
actionFromView Unmatched = Matched
actionFromView _         = Global

appChooseCursor :: AppState -> [CursorLocation String] -> Maybe (CursorLocation String)
appChooseCursor state locs = case T.view state of
  FileBrowser _ -> (\c -> c{cursorLocationVisible =True}) <$> find ((==Just "FileBrowser") . cursorLocationName) locs
  NewTorrentForm _ -> listToMaybe locs
  _ -> case visibleMenu state of
                  NoMenu -> Just $ CursorLocation {
                                cursorLocation = Location (1, mainCursor state + 3), -- accounting for the borders
                                cursorLocationName = Nothing,
                                cursorLocationVisible = False
                                            }
                  _ -> Just $ CursorLocation {
                                cursorLocation = Location (1, menuCursor state + 1), --accounting for the borders
                                cursorLocationName = Nothing,
                                cursorLocationVisible = False
                                       }

highlightRow :: Int -> [Widget n] -> [Widget n]
highlightRow cursor = zipWith (\ i w -> (if i == cursor then highlight w else w)) [0..]

highlight :: Widget n -> Widget n
highlight = withAttr cursorAttr

mkDialog :: DialogContent -> Int -> Dialog (Maybe DialogContent) String
mkDialog (Alert text) = dialog (Just . txt $ text) (Just ("OK", [("OK", "0", Nothing)]))
mkDialog r@(Remove (t, _)) = dialog
  (Just . str $ "Are you sure you want to remove " ++ show (length t) ++ " selected torrents")
  (Just ("0", [("No", "0", Nothing), ("Yes", "1", Just r)]))

withBrickLogger :: MonadUnliftIO m => BChan Events -> (Logger -> m r) -> m r
withBrickLogger chan act = withRunInIO $ \unlift -> do
  logger <- mkLogger "Brick" $ \msg -> do
    writeBChan chan . LogEvent $ showLogMessage Nothing msg
  withLogger logger (unlift . act)
