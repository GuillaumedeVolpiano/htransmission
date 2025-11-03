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
import           Effectful.Concurrent.STM   (newTChanIO, newTVarIO, newTChan, atomically)
import           Effectful.FileSystem       (runFileSystem)
import           Effectful.Log              (LogLevel (LogTrace), runLog)
import           Effectful.Matcher          (startMatcher)
import           Effectful.Prim.IORef       (runPrim)
import           Effectful.Reader.Static    (runReader)
import           Effectful.Time             (runTime)
import           Effectful.Timer            (startTimer)
import           Effectful.Types            (newClient, newMatcher)
import           Effectful.Unix             (runUnix)
import           Effectful.Wreq             (runWreq)
import           Graphics.Vty               (defaultConfig)
import           Graphics.Vty.Platform.Unix (mkVty)
import           Log.Backend.Text           (withSimpleTextLogger)
import           Options.Applicative        (Parser, execParser, fullDesc, help,
                                             helper, info, long, metavar,
                                             progDesc, short, strOption, value,
                                             (<**>))
import           Transmission.RPC.Client    (fromUrl)
import           UI.Constants
import           UI.KeyEvents               (dispatcher, keyConfig)
import           UI.Types                   (newState, newClientState)

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
  timerChan <- runEff . runConcurrent $ atomically newTChan
  runEff . runPrim . runConcurrent $ startTimer timerChan
  (clientState, matcherChan, matcherInVar, matcherOutVar, req) <- runEff . runConcurrent $ do
    cs <- newTVarIO newClientState
    mc <- newTChanIO
    miv <- newTVarIO mempty
    mov <- newTVarIO []
    r <- newTChanIO
    pure (cs, mc, miv, mov, r)
  vty <- mkVty defaultConfig
  client <- runEff . runWreq . runPrim $ fromUrl url Nothing Nothing
  let matcher = newMatcher matcherChan matcherOutVar matcherInVar
      localClient = newClient matcherChan matcherOutVar matcherInVar timerChan clientState req chan
  runEff . runConcurrent .runUnix $ startMatcher matcher
  (eventLog, _) <- withSimpleTextLogger $ \stl -> runEff .runConcurrent . runReader client . runWreq
    . runPrim . runLog "Client" stl LogTrace . runTime . runUnix . runFileSystem $ startClient localClient
  let appState = newState keyConfig dispatcher clientState req 
  void $ customMain vty (mkVty defaultConfig) (Just chan) app appState
  T.putStrLn eventLog
