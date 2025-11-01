{-# LANGUAGE OverloadedStrings #-}
module UI.Views (
  mkView
  )
where
import           Brick                     (Padding (..), Widget, emptyWidget,
                                            fill, hBox, hLimit, hLimitPercent,
                                            padBottom, padLeft, padRight, str,
                                            txt, vBox, vLimit, withAttr, (<+>),
                                            (<=>))
import           Brick.Widgets.Border      (borderWithLabel, hBorder)
import           Brick.Widgets.Core        (joinBorders)
import           Brick.Widgets.Dialog      (renderDialog)
import           Brick.Widgets.ProgressBar (progressBar)
import           Data.IntSet               (IntSet, member)
import           Data.Maybe                (fromJust, fromMaybe, isJust,
                                            isNothing, mapMaybe)
import           Data.Ratio                ((%))
import qualified Data.Text                 as T (intercalate)
import           Data.Time.Clock           (UTCTime (utctDay),
                                            nominalDiffTimeToSeconds)
import           Data.Time.Clock.POSIX     (posixSecondsToUTCTime)
import           Data.Time.Format.ISO8601  (iso8601Show)
import           Transmission.RPC.Session  (Session, SessionStats, currentStats,
                                            downloadSpeed, downloadedBytes,
                                            speedLimitDown,
                                            speedLimitDownEnabled, speedLimitUp,
                                            speedLimitUpEnabled, uploadSpeed,
                                            uploadedBytes)
import           Transmission.RPC.Torrent  (ETA (ETA, NA, Unknown), Torrent,
                                            activityDate, addedDate, comment,
                                            doneDate, downloadDir,
                                            downloadedEver, errorString, eta,
                                            hashString, isPrivate, labels, name,
                                            peers, peersConnected,
                                            peersGettingFromUs,
                                            peersSendingToUs, progress,
                                            rateDownload, rateUpload, ratio,
                                            toId, totalSize, uploadedEver,
                                            webseeds, webseedsSendingToUs)
import           UI.Attrs                  (selectedAttr)
import qualified UI.Types                  as UT (torrents)
import           UI.Types                  (AppState, Menu (NoMenu, Sort),
                                            View (..), mainCursor, mainOffset,
                                            mainVisibleHeight, menuCursor,
                                            selected, session, sessionStats,
                                            view, visibleDialog, visibleMenu)
import           UI.Utils                  (highlightRow)

mkView :: AppState -> [Widget String]
mkView s = pure .
  showDialog . showMenu $ mainWidget
  where
    mainWidget
      | view s == Unmatched = matchedView Unmatched (mainCursor s) (UT.torrents s) (session s)
        (sessionStats s) (selected s) vh (mainOffset s)
      | isJust tid = singleView (fromJust tid) (UT.torrents s) (session s) (sessionStats s) vh
      | otherwise = mainView (view s) (mainCursor s) (UT.torrents s) (session s) (sessionStats s) (selected s) vh
        (mainOffset s)
    showMenu = case visibleMenu s of
                 NoMenu -> joinBorders
                 Sort   -> \w -> joinBorders (sortMenu (menuCursor s) (mainVisibleHeight s) <+> w)
    showDialog = maybe id renderDialog . visibleDialog $ s
    vh = if isNothing (visibleDialog s) then mainVisibleHeight s else mainVisibleHeight s - 3
    tid = case view s of
            SingleTorrent i _ -> Just i
            _                 -> Nothing

mainTorrentsView :: Int -> IntSet -> [Torrent] -> Widget String
mainTorrentsView mc selection = foldr (<=>) emptyWidget . highlightRow mc . map (vLimit 1 . mainTorrentView selection)

mainHeader :: Ord n => Widget n
mainHeader = vLimit 1 (str "       " <+> hLimit 50 (padRight Max $ str "Name")
  <+> hLimit 16 (padRight Max $ str "Percent complete") <+> str "   "
  <+> hLimit 10 (padRight Max $ str "Downloaded") <+> str "   "
  <+> hLimit 16 (padRight Max $ str "Download speed") <+> str "   "
  <+> hLimit 10 (padRight Max $ str "Uploaded") <+> str "   "
  <+> hLimit 16 (padRight Max $ str "Upload speed") <+> str "   "
  <+> hLimit 8 (padRight Max $ str "ETA") <+> str "   "
  <+> hLimit 5 (padRight Max $ str "Ratio") <+> str "   "
  <+> hLimit 10 (padRight Max $ str "Total size") <+> str "   "
  <+> hLimit 10 (padRight Max $ str "Peers") <+> str "   "
  <+> hLimit 10 (padRight Max $ str "Seeds") <+> str "   "
  <+> hLimit 10 (padRight Max $ str "Date added") <+> str "   "
  <+> hLimit 20 (padRight Max $ str "Labels"))

mainTorrentView :: Ord n => IntSet -> Torrent -> Widget n
mainTorrentView selection torrent = select selection torrent $
  hLimit 50 (padRight Max (txt . fromMaybe "" $ name torrent)) <+> str "   "
  <+> hLimit 16 (padLeft Max (percentView . fromMaybe 0 $ progress torrent)) <+> str "   "
  <+> hLimit 10 (padLeft Max (sizeView . fromMaybe 0 $ downloadedEver torrent)) <+> str "   "
  <+> hLimit 14 (padLeft Max (sizeView . fromMaybe 0 $ rateDownload torrent) <+> str "/s")  <+> str "   "
  <+> hLimit 10 (padLeft Max (sizeView . fromMaybe 0 $ uploadedEver torrent)) <+> str "   "
  <+> hLimit 14 (padLeft Max (sizeView . fromMaybe 0 $ rateUpload torrent) <+> str "/s") <+> str "   "
  <+> hLimit 8 (padLeft Max (etaView . fromMaybe NA $ eta torrent)) <+> str "   "
  <+> hLimit 5 (padLeft Max (ratioView  $ ratio torrent)) <+> str "   "
  <+> hLimit 10 (padLeft Max (sizeView . fromMaybe 0 $ totalSize torrent)) <+> str "   "
  <+> hLimit 10 (padLeft Max (peersView (fromMaybe 0 $ peersConnected torrent)
    (length . fromMaybe [] $ peers torrent))) <+> str "   "
  <+> hLimit 10 (padLeft Max (peersView (fromMaybe 0 $ webseedsSendingToUs torrent)
    (length . fromMaybe [] $ webseeds torrent))) <+> str "   "
  <+> hLimit 10 (padRight Max
    (str . iso8601Show . utctDay . fromMaybe (posixSecondsToUTCTime 0) $ addedDate torrent)) <+> str "   "
  <+> hLimit 20 (padRight Max (txt . T.intercalate " " . fromMaybe [] $ labels torrent))

select :: Ord n => IntSet -> Torrent -> Widget n -> Widget n
select selection torrent
      | fromJust (toId torrent) `member` selection =
        \w ->hLimit 3 (str "[*]") <+> str "   " <+> withAttr selectedAttr w
      | otherwise = \w -> hLimit 3 (str "[ ]") <+> str "   " <+> w

matchedTorrentsView :: Int -> IntSet -> [Torrent] -> Widget String
matchedTorrentsView mainCounter selection = foldr (<=>) emptyWidget . highlightRow mainCounter . map (vLimit 1 . matchedTorrentView selection)

matchedHeader :: Ord n => Widget n
matchedHeader = vLimit 1 (str "       " <+> hLimit 50 (padRight Max $ str "Name")
  <+> hLimit 10 (padRight Max $ str "Downloaded") <+> str "   "
  <+> hLimit 10 (padRight Max $ str "Uploaded") <+> str "   "
  <+> hLimit 16 (padRight Max $ str "Upload speed") <+> str "   "
  <+> hLimit 5 (padRight Max $ str "Ratio") <+> str "   "
  <+> hLimit 10 (padRight Max $ str "Total size") <+> str "   "
  <+> hLimit 10 (padRight Max $ str "Peers") <+> str "   "
  <+> hLimit 10 (padRight Max $ str "Seeds") <+> str "   "
  <+> hLimit 10 (padRight Max $ str "Date added") <+> str "   "
  <+> hLimit 20 (padRight Max $ str "Labels"))

matchedTorrentView :: Ord n => IntSet -> Torrent -> Widget n
matchedTorrentView selection torrent = select selection torrent $
  hLimit 50 (padRight Max (txt . fromMaybe "" $ name torrent)) <+> str "   "
  <+> hLimit 10 (padLeft Max (sizeView . fromMaybe 0 $ downloadedEver torrent)) <+> str "   "
  <+> hLimit 10 (padLeft Max (sizeView . fromMaybe 0 $ uploadedEver torrent)) <+> str "   "
  <+> hLimit 14 (padLeft Max (sizeView . fromMaybe 0 $ rateUpload torrent) <+> str "/s") <+> str "   "
  <+> hLimit 5 (padLeft Max (ratioView  $ ratio torrent)) <+> str "   "
  <+> hLimit 10 (padLeft Max (sizeView . fromMaybe 0 $ totalSize torrent)) <+> str "   "
  <+> hLimit 10 (padLeft Max (peersView (fromMaybe 0 $ peersConnected torrent) (length . fromMaybe [] $ peers torrent)))
  <+> hLimit 10 (padLeft Max (peersView (fromMaybe 0 $ webseedsSendingToUs torrent) (length . fromMaybe [] $ webseeds torrent))) <+> str "   "
  <+> hLimit 10 (padRight Max (str . iso8601Show . utctDay . fromMaybe (posixSecondsToUTCTime 0) $ addedDate torrent)) <+> str "   "
  <+> hLimit 20 (padRight Max (txt . T.intercalate " " . fromMaybe [] $ labels torrent))

percentView :: Ord n => Rational -> Widget n
percentView pc = progressBar (Just (show (round pc :: Int) ++ " %")) (fromRational pc / 100)

sizeView :: Ord n => Int -> Widget n
sizeView s
  | s <= 2^(10 :: Int)  = str (show s ++ " B")
  | s <= 2^(20 :: Int) = str (truncated s 10 ++ " kB")
  | s <= 2^(30 :: Int) = str (truncated s 20 ++ " MB")
  | s <= 2^(40 :: Int) = str (truncated s 30 ++ " GB")
  | s <= 2^(50 :: Int) = str (truncated s 40 ++ " TB")
  | otherwise = str (truncated s 50 ++ " PB")
  where
    truncated :: Int -> Int -> String
    truncated a b = truncateDouble $ fromIntegral a / (2^b)
    truncateDouble :: Double -> String
    truncateDouble v
      | v >= 1000 = show (round v :: Int)
      | v >= 100 = take 3 . show $ v
      | v == (fromIntegral (round v :: Int) :: Double) = show (round v :: Int)
      | otherwise = take 4 . show $ v

etaView :: Ord n => ETA -> Widget n
etaView NA = padRight Max . str $ "∞"
etaView (ETA t)
          | weeks > 0 = padLeft Max . str $ show weeks ++ " w " ++ show ds ++ " d"
          | days > 0 = padLeft Max . str $ show days ++ " d " ++ show hrs ++ " h"
          | hours > 0 = padLeft Max . str $ show hours ++ " h " ++ show mins ++ " m"
          | minutes > 0 = padLeft Max . str $ show minutes ++ ":" ++ show secs
          | otherwise = padLeft Max . str $ show secs ++ " s"
          where
            seconds = round . nominalDiffTimeToSeconds $ t :: Int
            secs = seconds `mod` 60
            minutes = div seconds 60
            mins = minutes `mod` 60
            hours = div minutes 60
            hrs = hours `mod` 24
            days =  div hours 24
            ds = days `mod` 7
            weeks = div days 7
etaView Unknown = padRight Max . str $ "unknown"

ratioView :: Ord n => Maybe Rational -> Widget n
ratioView r
  | isNothing r = str "∞"
  | r' >= 100 = str . show . (round :: RealFrac a => a -> Int) $ r'
  | r' <= 0.01 = str "0"
  | otherwise = str . take 4 . show $ r'
  where
    r' = fromRational . fromJust $ r :: Double

peersView :: Ord n => Int -> Int -> Widget n
peersView connected total = str $ show connected ++ " of " ++ show total

sessionView :: Ord n => Session -> SessionStats -> Widget n
sessionView sesh seshStats = padLeft Max (str "Down: ") <+> sizeView (downloadSpeed seshStats)
  <+> str "/s (" <+> sizeView db <+> str " | "
  <+> limitView (fromMaybe False $ speedLimitDownEnabled sesh) (fromMaybe 0 $ speedLimitDown sesh)
  <+> str ") Up: " <+> sizeView (uploadSpeed seshStats) <+> str "/s ("
  <+> sizeView ub <+> str " | "
  <+> limitView (fromMaybe False $ speedLimitUpEnabled sesh) (fromMaybe 0 $ speedLimitUp sesh)
  <+> str ") Ratio: " <+> ratioView (if fromIntegral db == (0 :: Int) then Nothing else Just (fromIntegral ub % fromIntegral db)) <+> hLimit 5 (fill ' ')
    where
      ub = uploadedBytes . currentStats $ seshStats
      db = downloadedBytes . currentStats $ seshStats

torrentData :: Ord n => [Torrent] -> Widget n
torrentData torrents = hLimitPercent 50 . padRight Max $
  str (show . length $ torrents) <+> str " torrents   " <+> str "Total size: " <+> sizeView totSize

  where
    totSize = sum . mapMaybe totalSize $ torrents

limitView :: Ord n => Bool -> Int -> Widget n
limitView False _ = str "∞ "
limitView _ lim   = sizeView lim <+> str "/s "

mainView :: View -> Int -> [Torrent] -> Session -> SessionStats -> IntSet -> Int -> Int -> Widget String
mainView v mc torrents sesh seshStats selection visibleHeight offset =
      borderWithLabel (str . show $ v) $ mainHeader <=> hBorder
  <=> (vLimit visibleHeight . mainTorrentsView mc selection . take visibleHeight . drop offset $ torrents)
  <=> hBorder <=> vLimit 1 (sessionView sesh seshStats)

matchedView :: View -> Int -> [Torrent] -> Session -> SessionStats -> IntSet -> Int -> Int -> Widget String
matchedView v mc torrents sesh seshStats selection visibleHeight offset =
      joinBorders . borderWithLabel (str . show $ v) $ matchedHeader
  <=> hBorder <=> (vLimit visibleHeight . matchedTorrentsView mc selection . take visibleHeight. drop offset $ torrents)
  <=> hBorder <=> vLimit 1 (torrentData torrents <+> sessionView sesh seshStats)

sortMenu :: Int -> Int -> Widget n
sortMenu mc vh = (vLimit 2 . padBottom Max . hLimitPercent 10 . padRight Max . str $ " ")
  <=> (borderWithLabel (str "Sort by") . vLimit vh . padBottom Max
  . hLimit 20 . padRight Max . vBox . highlightRow mc $ [str "Name", str "Percent complete"
  , str "Downloaded", str "Download speed", str "Uploaded", str "Upload speed", str "ETA", str "Ratio"
  , str "Total size", str "Peers", str "Seeds", str "Date added", str "Labels"])

singleView :: Int -> [Torrent] -> Session -> SessionStats -> Int -> Widget String
singleView idx torrents sesh seshStats vh = borderWithLabel (txt . fromJust . name $ torrent) $ singleHeader torrent
  <=> vLimit vh (padBottom Max . singleTorrentView $ torrent) <=> hBorder
  <=> vLimit 1 (singleTorrentData idx (length torrents) <+> sessionView sesh seshStats)
  where
    torrent = torrents !! idx

singleTorrentView :: Torrent -> Widget String
singleTorrentView torrent = hBox [hLimit 50 . padRight Max . vBox $ [str "General", str "Added", str "Location"
                                                                    , str "Labels", str " ", str "Transfer"
                                                                    , str "Finished", str "Downloaded"
                                                                    , str "Peers", str "Seeds", str "Last Activity"
                                                                    , str " ", str "Torrent", str "Created"
                                                                    , str "Hash", str "Size", str "Type"
                                                                    , str "Comment", str " ", str "Tracker"
                                                                    , str "Tracker Message"]
                                 , padRight Max . vBox $ [str " ", str . show . fromJust . addedDate $ torrent
                                                        , str . fromJust . downloadDir $ torrent
                                                        , txt . T.intercalate "," . fromJust . labels $ torrent
                                                        , str " ", str " "
                                                        , str . maybe "None" show . doneDate $ torrent
                                                        , hBox [str . show . maybe (0 :: Int) round .  progress 
                                                                  $ torrent
                                                               , str " %"]
                                                        , hBox [str . show $ 
                                                                  (fromMaybe 0 . peersSendingToUs $ torrent) 
                                                                  + (fromMaybe 0 . peersGettingFromUs $ torrent)
                                                               , str " connected of " 
                                                               , str . show . maybe 0 length . peers $ torrent]
                                                        , hBox [str . show . fromMaybe 0 . webseedsSendingToUs $ 
                                                                  torrent
                                                               , str " connected of "
                                                               , str . show . maybe 0 length . webseeds $ torrent]
                                                        , str . show . fromJust . activityDate $ torrent
                                                        , str " ", str " "
                                                        , str . maybe "None" show . addedDate $ torrent
                                                        , txt . fromMaybe "None" . hashString $ torrent
                                                        , sizeView . fromMaybe 0 . totalSize $ torrent
                                                        , str $ if isPrivate torrent == Just True 
                                                                   then "Private" 
                                                                   else "Public"
                                                        , txt . fromMaybe "None" . comment $ torrent
                                                        , str " ", str " "
                                                        , txt . fromMaybe "None" . errorString $ torrent]]

singleHeader :: Torrent -> Widget String
singleHeader torrent = vBox [
  vLimit 1 . hBox $ [sizeView . fromMaybe 0 . rateDownload $ torrent, str "/s"
                                     , str " − ", sizeView . fromMaybe 0 . downloadedEver $ torrent
                                     , str "   ", sizeView . fromMaybe 0 . rateUpload $ torrent, str "/s"
                                     , str " - ", sizeView . fromMaybe 0 . uploadedEver $ torrent, str "   "
                                     , str . show . maybe (0 :: Int) round . ratio $ torrent, str "   " 
                                     , hLimit 10 . etaView . fromJust . eta $ torrent]
                            , vLimit 1 . percentView . fromMaybe 0 . progress $ torrent]


singleTorrentData :: Int -> Int -> Widget String
singleTorrentData idx totSize = hLimitPercent 50 . padRight Max $ str (show (idx + 1) ++ '/':show totSize)
