{-# LANGUAGE OverloadedStrings #-}
module UI.Views (
  mkView
  )
where
import           Brick.Widgets.Border       (border)
import           Brick.Widgets.Border.Style (BorderStyle (..),
                                             defaultBorderStyle)
import           Brick.Widgets.ProgressBar  (progressBar)
import           Brick.Widgets.Table        (ColumnAlignment (..), renderTable,
                                             rowBorders, setColAlignment,
                                             setDefaultColAlignment, table)
import           Data.Maybe                 (fromMaybe)
import           Data.Ratio                 ((%))
import qualified Data.Text                  as T (intercalate)
import           Data.Time.Clock            (UTCTime (utctDay),
                                             nominalDiffTimeToSeconds)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import           Data.Time.Format.ISO8601   (iso8601Show)
import           Brick            (Padding (..), Widget, hLimit,
                                             padLeft, padRight, str, txt,
                                             vLimit, vLimitPercent,
                                             withBorderStyle, (<+>), (<=>))
import           Transmission.RPC.Session   (Session, SessionStats,
                                             currentStats, downloadSpeed,
                                             downloadedBytes, speedLimitDown,
                                             speedLimitDownEnabled,
                                             speedLimitUp, speedLimitUpEnabled,
                                             uploadSpeed, uploadedBytes)
import           Transmission.RPC.Torrent   (ETA (ETA, NA, Unknown), Torrent,
                                             addedDate, downloadedEver, eta,
                                             labels, name, peers,
                                             peersConnected, percentComplete,
                                             rateDownload, rateUpload, ratio,
                                             totalSize, uploadedEver, webseeds,
                                             webseedsSendingToUs)
import Types (AppState(AppState), View (..))


mkView :: AppState -> [Widget Int]
mkView (AppState Main t s ss) = [mainView t s ss]
mkView (AppState Prune _ _ _) = undefined

emptyBordersStyle :: BorderStyle
emptyBordersStyle = defaultBorderStyle {bsVertical = ' '
                                  , bsHorizontal = ' '
                                  , bsIntersectT = ' '
                                  , bsCornerTL = ' '
                                  , bsCornerTR = ' '
                                  , bsCornerBL = ' '
                                  , bsCornerBR = ' '
                                 }

mainTorrentsView :: Ord n => [Torrent] -> Widget n
mainTorrentsView torrents = withBorderStyle emptyBordersStyle
                                        . renderTable . rowBorders False
                                        . setDefaultColAlignment AlignRight
                                        . setColAlignment AlignLeft 0
                                        . setColAlignment AlignCenter 1
                                        . setColAlignment AlignCenter 6
                                        . setColAlignment AlignLeft 12
                                        . table $ (map (vLimit 1) [hLimit 40 $ str "Name"
                                                                  , hLimit 16 $ str "Percent complete"
                                                                  , hLimit 10 $ str "Downloaded"
                                                                  , hLimit 16 $ str "Download speed"
                                                                  , hLimit 10 $ str "Uploaded"
                                                                  , hLimit 16 $ str "Upload speed"
                                                                  , hLimit 8 $ str "ETA"
                                                                  , hLimit 5 $ str "Ratio"
                                                                  , hLimit 10 $ str "Total size"
                                                                  , hLimit 10 $ str "Peers"
                                                                  , hLimit 10 $ str "Seeds"
                                                                  , hLimit 10 $ str "Date added"
                                                                  , hLimit 20 $ str "Labels"] :
                                                                  map (map (vLimit 1) . mainTorrentView) torrents)

mainTorrentView :: Ord n => Torrent -> [Widget n]
mainTorrentView torrent = [hLimit 40 (str . fromMaybe "" $ name torrent)
  , hLimit 16 . percentView . fromMaybe 0 $ percentComplete torrent
  , hLimit 10 . sizeView . fromMaybe 0 $ downloadedEver torrent
  , (hLimit 14 . sizeView . fromMaybe 0 $ rateDownload torrent) <+> str "/s"
  , hLimit 10 . sizeView . fromMaybe 0 $ uploadedEver torrent
  , (hLimit 14 . sizeView . fromMaybe 0 $ rateUpload torrent) <+> str "/s"
  , hLimit 8 . etaView . fromMaybe NA $ eta torrent
  , hLimit 5 . ratioView . fromMaybe 0 $ ratio torrent
  , hLimit 10 . sizeView . fromMaybe 0 $ totalSize torrent
  , hLimit 10 . peersView (fromMaybe 0 $ peersConnected torrent)
      $ (length . fromMaybe [] $ peers torrent)
  , hLimit 10 . peersView (fromMaybe 0 $ webseedsSendingToUs torrent)
      $ (length . fromMaybe [] $ webseeds torrent)
  , hLimit 10 . str . iso8601Show . utctDay . fromMaybe (posixSecondsToUTCTime 0) $ addedDate torrent
  , hLimit 20 . txt . T.intercalate " " . fromMaybe [] $ labels torrent]

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
sessionView sesh seshStats = vLimit 1 $ str "Down: " <+> sizeView (downloadSpeed seshStats)
  <+> str "/s (" <+> sizeView db <+> str " | "
  <+> limitView (fromMaybe False $ speedLimitDownEnabled sesh) (fromMaybe 0 $ speedLimitDown sesh)
  <+> str ") Up: " <+> sizeView (uploadSpeed seshStats) <+> str "/s ("
  <+> sizeView ub <+> str " | "
  <+> limitView (fromMaybe False $ speedLimitUpEnabled sesh) (fromMaybe 0 $ speedLimitUp sesh)
  <+> str ") Ratio: " <+> ratioView (fromIntegral ub % fromIntegral db)
    where
      ub = uploadedBytes . currentStats $ seshStats
      db = downloadedBytes . currentStats $ seshStats

limitView :: Ord n => Bool -> Int -> Widget n
limitView False _ = str "∞ "
limitView _ lim   = sizeView lim <+> str "/s "

mainView :: [Torrent] -> Session -> SessionStats -> Widget Int
mainView torrents sesh seshStats = (vLimitPercent 90 . border . mainTorrentsView $ torrents)  <=> sessionView sesh seshStats
