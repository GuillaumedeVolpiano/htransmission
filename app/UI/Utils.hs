module UI.Utils
  (sel
  , actionFromView
  , appChooseCursor
  , highlightRow)
where

import           Brick                    (CursorLocation (CursorLocation, cursorLocation, cursorLocationName, cursorLocationVisible),
                                           Location (Location), Widget,
                                           withAttr)
import           Data.Maybe               (fromMaybe)
import           Transmission.RPC.Torrent (Torrent, errorCode, progress,
                                           rateDownload, rateUpload, status)
import qualified Transmission.RPC.Types   as TT (Error (OK),
                                                 Status (Downloading, Seeding, Stopped))
import           Types                    (Action (Global, Matched))
import           UI.Attrs                 (cursorAttr)
import           UI.Types                 (AppState, Menu (NoMenu),
                                           View (Complete, Downloading, Error, Inactive, Main, Paused, Prune, Seeding),
                                           mainCursor, menuCursor, torrents,
                                           view, visibleMenu, sortKey, reverseSort)
import Utils (sortTorrents)

sel :: AppState -> [Torrent]
sel state = sortTorrents (sortKey state) (reverseSort state). filter selector . torrents $ state
  where
  selector = case view state of
      Main        -> const True
      Downloading -> (== Just TT.Downloading) . status
      Seeding     -> (== Just TT.Seeding) . status
      Complete    -> (==100) . fromMaybe 0 . progress
      Paused      -> (== Just TT.Stopped) . status
      Inactive    -> (\t -> rateDownload t == Just 0 && rateUpload t == Just 0)
      Error       -> (/= Just TT.OK) . errorCode
      Prune       -> const True

actionFromView :: View -> Action
actionFromView Prune = Matched
actionFromView _     = Global

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
