module UI.Utils
  (sel
  , actionFromView
  , appChooseCursor
  , highlightRow
  , mkDialog)
where

import           Brick                    (CursorLocation (CursorLocation, cursorLocation, cursorLocationName, cursorLocationVisible),
                                           Location (Location), Widget, str,
                                           txt, withAttr)
import           Brick.Widgets.Dialog     (Dialog, dialog)
import           Data.IntSet              (IntSet, member)
import           Data.Maybe               (fromJust, fromMaybe)
import           Transmission.RPC.Torrent (Torrent, errorCode, progress,
                                           rateDownload, rateUpload, status,
                                           toId)
import qualified Transmission.RPC.Types   as TT (Error (OK),
                                                 Status (Downloading, Seeding, Stopped))
import           Types                    (Action (Global, Matched), Sort)
import           UI.Attrs                 (cursorAttr)
import           UI.Types                 (AppState,
                                           DialogContent (Alert, Remove),
                                           Menu (NoMenu),
                                           View (Complete, Downloading, Error, Inactive, Main, Paused, Seeding, SingleTorrent, Unmatched),
                                           getView, mainCursor, menuCursor,
                                           visibleMenu)
import           Utils                    (sortTorrents)

sel :: View -> IntSet -> Sort -> Bool -> [Torrent] -> [Torrent]
sel view unmatched sortKey reverseSort = sortTorrents sortKey reverseSort. filter selector
  where
  selector = case getView view of
      Main              -> const True
      Downloading       -> (== Just TT.Downloading) . status
      Seeding           -> (== Just TT.Seeding) . status
      Complete          -> (==100) . fromMaybe 0 . progress
      Paused            -> (== Just TT.Stopped) . status
      Inactive          -> (\t -> rateDownload t == Just 0 && rateUpload t == Just 0)
      Error             -> (/= Just TT.OK) . errorCode
      Unmatched         -> flip member unmatched . fromJust . toId
      SingleTorrent {}  -> undefined

actionFromView :: View -> Action
actionFromView Unmatched = Matched
actionFromView _         = Global

appChooseCursor :: AppState -> [CursorLocation n] -> Maybe (CursorLocation n)
appChooseCursor state _ = case visibleMenu state of
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
