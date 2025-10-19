{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Brick                      (App (..), defaultMain,
                                             resizeOrQuit, showFirstCursor)
import           Brick.AttrMap              (attrMap)
import           Constants                  (arrPaths, basicSession,
                                             basicTorrents, pathMap)
import           Control.Monad              (void)
import qualified Data.Text                  as T (pack)
import           Effectful                  (Eff, IOE, runEff, (:>))
import           Effectful.Log              (LogLevel (LogTrace), runLog)
import           Effectful.Prim.IORef       (Prim, runPrim)
import           Effectful.Reader.Static    (runReader)
import           Effectful.Time             (runTime)
import           Effectful.Unix             (runUnix)
import           Effectful.Wreq             (Wreq, runWreq)
import           Graphics.Vty               (defAttr)
import           Log.Backend.StandardOutput (withStdOutLogger)
import           Options.Applicative        (Parser, execParser, fullDesc, help,
                                             helper, info, long, metavar,
                                             progDesc, short, strOption, value,
                                             (<**>))
import           Transmission.RPC.Client    (Client, fromUrl, getSession,
                                             getTorrents, sessionStats)
import           Transmission.RPC.Session   (Session, SessionStats)
import           Transmission.RPC.Torrent   (Torrent)
import           Types                      (AppState (AppState), View (Main))
import           UI.Views                   (mkView)
import           Utils                      (extractPrunable, mkPathMap)

newtype Args = Args {
                 getHost :: String
                 }

app :: App AppState () Int
app = App {
        appDraw = mkView,
        appChooseCursor = showFirstCursor,
        appHandleEvent = resizeOrQuit,
        appStartEvent = pure (),
        appAttrMap = const $ attrMap defAttr []
          }


args :: Parser Args
args =
  Args <$> strOption
             (long "host" <> short 'h' <> metavar "Host" <> value "http://localhost:9091/transmission/rpc" <> help "link to the transmission host, like http://username:password@host:9091/transmission/rpc")

getTorrentsSessionAndStats :: (IOE :> es, Wreq :> es, Prim :> es) => Client -> Eff es ([Torrent], Session, SessionStats)
getTorrentsSessionAndStats client = withStdOutLogger $ \stdoutLogger -> do
                                      runReader client . runLog (T.pack "htransmission") stdoutLogger LogTrace . runTime $ do
                                        t <- getTorrents Nothing (Just basicTorrents) Nothing
                                        s <- getSession (Just basicSession) Nothing
                                        sst <- sessionStats Nothing
                                        pure (t, s, sst)


main :: IO ()
main = do
  (Args url) <- execParser . info (args <**> helper) $ (fullDesc <> progDesc "A command line to communicate with transmission-daemon")
  (torrents, sesh, seshStats) <- runEff . runWreq . runPrim . runUnix $ do
                                                                      client <- fromUrl url Nothing Nothing
                                                                      (torrents, sesh, seshStats) <- getTorrentsSessionAndStats client
                                                                      prunables <- extractPrunable arrPaths (mkPathMap pathMap) torrents
                                                                      pure (prunables, sesh, seshStats)
  let appState = AppState Main torrents sesh seshStats
  void $ defaultMain app appState
