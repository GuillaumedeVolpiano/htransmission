{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text                  (Text)
import qualified Data.Text                  as T (pack)
import           Effectful                  (runEff)
import           Effectful.Brick            (runBrick, simpleMain)
import           Effectful.Log              (LogLevel (LogTrace), runLog)
import           Effectful.Prim.IORef       (runPrim)
import           Effectful.Reader.Static    (runReader)
import           Effectful.Time             (runTime)
import           Effectful.Wreq             (runWreq)
import           Log.Backend.StandardOutput (withStdOutLogger)
import           Options.Applicative        (Parser, execParser, fullDesc, help,
                                             helper, info, long, metavar,
                                             progDesc, short, strOption, value,
                                             (<**>))
import           Transmission.RPC.Client    (fromUrl, getTorrents, getSession, sessionStats)
import           UI.Views                   (mainView)

newtype Args = Args {
                 getHost :: String
                 }

basicTorrents :: [Text]
basicTorrents = ["name", "downloadedEver", "rateDownload", "uploadedEver", "rateUpload", "eta", "uploadRatio", "totalSize", "peers", "webseeds", "dateCreated", "percentComplete", "labels"]

basicSession :: [Text]
basicSession = ["speed-limit-down-enabled", "speed-limit-down", "speed-limit-up-enabled", "speed-limit-up"]

args :: Parser Args
args =
  Args <$> strOption
             (long "host" <> short 'h' <> metavar "Host" <> value "http://localhost:9091/transmission/rpc" <> help "link to the transmission host, like http://username:password@host:9091/transmission/rpc")

main :: IO ()
main = do
  (Args url) <- execParser . info (args <**> helper) $ (fullDesc <> progDesc "A command line to communicate with transmission-daemon")
  runEff . runWreq . runPrim . runBrick $ do
                                 client <- fromUrl url Nothing Nothing
                                 (torrents, sesh, seshStats) <- withStdOutLogger $ \stdoutLogger -> do
                                    runReader client . runLog (T.pack "htransmission") stdoutLogger LogTrace . runTime $ do
                                      t <- getTorrents Nothing (Just basicTorrents) Nothing
                                      s <- getSession (Just basicSession) Nothing
                                      sst <- sessionStats Nothing
                                      pure (t, s, sst)
                                 simpleMain . mainView torrents sesh $ seshStats 
