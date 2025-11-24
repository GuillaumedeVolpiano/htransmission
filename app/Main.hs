{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}


module Main where

import           Brick                         (customMain)
import           Brick.BChan                   (newBChan, writeBChan)
import           Constants                     (arrPaths)
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.Async      (race_)
import           Control.Concurrent.STM        (atomically, newTBQueueIO,
                                                newTChanIO, newTVarIO,
                                                readTChan, writeTChan)
import           Control.Monad                 (forever, void)
import           Data.IORef                    (newIORef)
import qualified Data.Text.IO                  as T
import           Effectful                     (runEff)
import           Effectful.Concurrent          (runConcurrent)
import           Effectful.Log                 (LogLevel (LogTrace), mkLogger,
                                                showLogMessage)
import           Effectful.Prim.IORef          (runPrim)
import           Effectful.Transmission.RPC.Client (runFullClient)
import           Effectful.RPCClient           (runRPCClient)
import           Graphics.Vty                  (defaultConfig)
import           Graphics.Vty.Platform.Unix    (mkVty)
import           Log.Monad                     (runLogT)
import           Network.HTTP.Client           (defaultManagerSettings,
                                                newManager)
import           Options.Applicative           (Parser, execParser, fullDesc,
                                                help, helper, info, long,
                                                metavar, progDesc, short,
                                                strOption, value, (<**>))
import qualified Streamly.Generators           as S (timer, uiBUS, watcher)
import           Streamly.Matcher              (buildArrs, runMatcher)
import           System.IO                     (stderr)
import           Types                         (Events (LogEvent),
                                                Matcher (Matcher),
                                                newClientState, newState)
import           UI.Client                     (startClient)
import           UI.Constants                  (addForm, app, fileBrowser)
import           UI.KeyEvents                  (dispatcher, keyConfig)

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
  clientState <- newTVarIO newClientState
  req <- newTChanIO
  pay <- newTChanIO
  logChan <- newTChanIO
  eventQueue <- newTBQueueIO 130
  prunedVar <- newTVarIO mempty
  ais' <- newTVarIO mempty
  aim' <- newTVarIO mempty
  itd <- newTVarIO mempty
  tid <- newTVarIO mempty
  ct <- newIORef mempty
  pr <- newTVarIO False
  counter <- newIORef 0
  logger <- mkLogger "Brick" $ \msg -> do
    (writeBChan chan . LogEvent $ showLogMessage Nothing msg) >> atomically
      (writeTChan logChan $ showLogMessage Nothing msg)
  let timerStream = S.timer 5 counter req
      uiStream = S.uiBUS pay req
      watchStream = S.watcher arrPaths
      matcher = Matcher arrPaths ais' aim' itd tid prunedVar pr eventQueue
  vty <- mkVty defaultConfig
  fb <- fileBrowser
  manager <- newManager defaultManagerSettings
  let appState = newState keyConfig dispatcher addForm clientState pay fb
  void . forkIO . forever $ atomically (readTChan logChan) >>= T.hPutStrLn stderr
  let m = runLogT "Matcher" logger LogTrace $ runMatcher matcher [timerStream, uiStream, watchStream]
      ui = customMain vty (mkVty defaultConfig) (Just chan) app appState
  forkIO $ runLogT "arrBuilder" logger LogTrace $ buildArrs matcher
  void $ runEff .runConcurrent . runPrim . runFullClient url manager "Client" logger LogTrace
    . runRPCClient req clientState chan ct pr $ startClient
  race_ m ui
