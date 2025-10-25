{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Brick                      (customMain)
import           Brick.BChan                (newBChan)
import           Control.Monad              (void)
import qualified Data.Text.IO               as T (putStrLn)
import           Effectful                  (runEff)
import           Effectful.Client           (startClient)
import           Effectful.Concurrent       (runConcurrent)
import           Effectful.Concurrent.STM   (newTVarIO)
import           Effectful.Log              (LogLevel (LogTrace), runLog)
import           Effectful.Prim.IORef       (runPrim)
import           Effectful.Reader.Static    (runReader)
import           Effectful.Time             (runTime)
import           Effectful.Timer            (startTimer)
import           Effectful.Unix             (runUnix)
import           Effectful.Wreq             (runWreq)
import           Graphics.Vty               (defaultConfig)
import           Graphics.Vty.Platform.Unix (mkVty)
import           Log.Backend.Text           (withSimpleTextLogger)
import           Options.Applicative        (Parser, execParser, fullDesc, help,
                                             helper, info, long, metavar,
                                             progDesc, short, strOption, value,
                                             (<**>))
import           Prelude                    hiding (log)
import           Transmission.RPC.Client    (fromUrl)
import           Types                      (newFIFOSet)
import           UI.Constants
import           UI.KeyEvents               (dispatcher, keyConfig)
import           UI.Types                   (newState)

newtype Args = Args {
                 getHost :: String
                 }

args :: Parser Args
args =
  Args <$> strOption
             (long "host" <> short 'h' <> metavar "Host" <> value "http://localhost:9091/transmission/rpc" <> help "link to the transmission host, like http://username:password@host:9091/transmission/rpc")

main :: IO ()
main = do
  (Args url) <- execParser . info (args <**> helper) $ (fullDesc <> progDesc "A command line to communicate with transmission-daemon")
  chan <- newBChan 10
  runEff . runConcurrent $ startTimer chan
  queue <- runEff . runConcurrent . newTVarIO $ newFIFOSet
  vty <- mkVty defaultConfig
  client <- runEff . runWreq . runPrim $ fromUrl url Nothing Nothing
  (eventLog, _) <- withSimpleTextLogger $ \stl -> runEff .runConcurrent . runReader client . runWreq
    . runPrim . runLog "Client" stl LogTrace . runTime . runUnix $ startClient queue chan
  let appState = newState keyConfig dispatcher queue
  void $ customMain vty (mkVty defaultConfig) (Just chan) app appState
  T.putStrLn eventLog
