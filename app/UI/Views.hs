{-# LANGUAGE OverloadedStrings #-}
module UI.Views (
  mkView
  )
where
import           Brick                     (Padding (..), Widget, emptyWidget,
                                            fill, hLimit, hLimitPercent,
                                            padLeft, padRight, str, txt, vLimit,
                                            vLimitPercent, (<+>), (<=>))
import           Brick.Widgets.Border      (border, hBorder)
import           Brick.Widgets.ProgressBar (progressBar)
import           Data.Maybe                (fromMaybe)
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
                                            addedDate, downloadedEver,
                                            errorCode, eta, labels, name, peers,
                                            peersConnected, progress,
                                            rateDownload, rateUpload, ratio,
                                            status, totalSize, uploadedEver,
                                            webseeds, webseedsSendingToUs)
import qualified Transmission.RPC.Types    as TT (Error (..), Status (..))
import           Types                     (AppState (AppState), View (..))
import Brick.Widgets.Core (joinBorders)


mkView :: AppState -> [Widget Int]

mkView (AppState Prune _ _ _ _ _ _ _) = undefined
mkView (AppState view _ torrents sesh seshStats _ _ _) = 
  pure $ mainView (filter (sel view) torrents) sesh seshStats

sel :: View -> Torrent -> Bool
sel Main        = const True
sel Downloading = (== Just TT.Downloading) . status
sel Seeding     = (== Just TT.Seeding) . status
sel Complete    = (==100) . fromMaybe 0 . progress
sel Paused      = (== Just TT.Stopped) . status
sel Inactive    = \t -> rateDownload t == Just 0 && rateUpload t == Just 0
sel Error       = (/= Just TT.OK) . errorCode
sel Prune       = error "Thou shall not pass"

mainTorrentsView :: Ord n => [Torrent] -> Widget n
mainTorrentsView = foldr (\t v -> (vLimit 1 . mainTorrentView $ t) <=> v) emptyWidget

mainHeader :: Ord n => Widget n
mainHeader = vLimit 1 (str " " <+> hLimitPercent 30 (padRight Max $ str "Name")
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

mainTorrentView :: Ord n => Torrent -> Widget n
mainTorrentView torrent = hLimitPercent 30 (padRight Max (txt . fromMaybe "" $ name torrent)) <+> str "   "
  <+> hLimit 16 (padLeft Max (percentView . fromMaybe 0 $ progress torrent)) <+> str "   "
  <+> hLimit 10 (padLeft Max (sizeView . fromMaybe 0 $ downloadedEver torrent)) <+> str "   "
  <+> hLimit 14 (padLeft Max (sizeView . fromMaybe 0 $ rateDownload torrent) <+> str "/s")  <+> str "   "
  <+> hLimit 10 (padLeft Max (sizeView . fromMaybe 0 $ uploadedEver torrent)) <+> str "   "
  <+> hLimit 14 (padLeft Max (sizeView . fromMaybe 0 $ rateUpload torrent) <+> str "/s") <+> str "   "
  <+> hLimit 8 (padLeft Max (etaView . fromMaybe NA $ eta torrent)) <+> str "   "
  <+> hLimit 5 (padLeft Max (ratioView . fromMaybe 0 $ ratio torrent)) <+> str "   "
  <+> hLimit 10 (padLeft Max (sizeView . fromMaybe 0 $ totalSize torrent)) <+> str "   "
  <+> hLimit 10 (padLeft Max (peersView (fromMaybe 0 $ peersConnected torrent) (length . fromMaybe [] $ peers torrent))) <+> str "   "
  <+> hLimit 10 (padLeft Max (peersView (fromMaybe 0 $ webseedsSendingToUs torrent) (length . fromMaybe [] $ webseeds torrent))) <+> str "   "
  <+> hLimit 10 (padRight Max (str . iso8601Show . utctDay . fromMaybe (posixSecondsToUTCTime 0) $ addedDate torrent)) <+> str "   "
  <+> hLimit 20 (padRight Max (txt . T.intercalate " " . fromMaybe [] $ labels torrent))

percentView :: Ord n => Rational -> Widget n
percentView = progressBar Nothing . (/100) . fromRational

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
          | weeks > 0 = padLeft Max . str $ show weeks ++ " w " ++ show days ++ " d"
          | days > 0 = padLeft Max . str $ show days ++ " d " ++ show hours ++ " h"
          | hours > 0 = padLeft Max . str $ show hours ++ " h " ++ show mins ++ " m"
          | mins > 0 = padLeft Max . str $ show mins ++ ":" ++ show secs
          | otherwise = padLeft Max . str $ show secs ++ " s"
          where
            seconds = round . nominalDiffTimeToSeconds $ t :: Int
            secs = seconds `mod` 60
            mins = div secs 60 `mod` 60
            hours = div mins 60 `mod` 24
            days =  div hours 24 `mod` 7
            weeks = div days 7
etaView Unknown = padRight Max . str $ "unknown"

ratioView :: Ord n => Rational -> Widget n
ratioView r
  | r' >= 100 = str . show . (round :: RealFrac a => a -> Int) $ r'
  | r' <= 0.01 = str "0"
  | otherwise = str . take 4 . show $ r'
  where
    r' = fromRational r :: Double

peersView :: Ord n => Int -> Int -> Widget n
peersView connected total = str $ show connected ++ " of " ++ show total

sessionView :: Ord n => Session -> SessionStats -> Widget n
sessionView sesh seshStats = vLimit 1 $ padLeft Max (str "Down: ") <+> sizeView (downloadSpeed seshStats)
  <+> str "/s (" <+> sizeView db <+> str " | "
  <+> limitView (fromMaybe False $ speedLimitDownEnabled sesh) (fromMaybe 0 $ speedLimitDown sesh)
  <+> str ") Up: " <+> sizeView (uploadSpeed seshStats) <+> str "/s ("
  <+> sizeView ub <+> str " | "
  <+> limitView (fromMaybe False $ speedLimitUpEnabled sesh) (fromMaybe 0 $ speedLimitUp sesh)
  <+> str ") Ratio: " <+> ratioView (fromIntegral ub % fromIntegral db) <+> hLimit 5 (fill ' ')
    where
      ub = uploadedBytes . currentStats $ seshStats
      db = downloadedBytes . currentStats $ seshStats

limitView :: Ord n => Bool -> Int -> Widget n
limitView False _ = str "∞ "
limitView _ lim   = sizeView lim <+> str "/s "

mainView :: [Torrent] -> Session -> SessionStats -> Widget Int
mainView torrents sesh seshStats = joinBorders . border $ mainHeader <=> hBorder
  <=> (vLimitPercent 90 . mainTorrentsView $ torrents)
  <=> hBorder <=> sessionView sesh seshStats
