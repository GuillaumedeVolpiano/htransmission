{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Brick                      (customMain)
import           Brick.BChan                (newBChan)
import           Constants                  (arrPaths)
import           Control.Concurrent         (forkIO, getNumCapabilities)
import           Control.Concurrent.STM     (newTChanIO, newTVarIO)
import           Control.Monad              (void)
import           Data.Function              ((&))
import           Data.IORef                 (newIORef)
import           Effectful                  (runEff)
import           Effectful.Client           (runClient)
import           Effectful.Concurrent       (runConcurrent)
import           Effectful.FileSystem       (runFileSystem)
import           Effectful.Log              (LogLevel (LogTrace), runLog)
import           Effectful.Prim.IORef       (runPrim)
import           Effectful.Time             (runTime)
import           Effectful.Wreq             (runWreq)
import           Graphics.Vty               (defaultConfig)
import           Graphics.Vty.Platform.Unix (mkVty)
import           Log.Backend.Text           (withSimpleTextLogger)
import           Options.Applicative        (Parser, execParser, fullDesc, help,
                                             helper, info, long, metavar,
                                             progDesc, short, strOption, value,
                                             (<**>))
import qualified Streamly.Data.Fold         as F (drain)
import qualified Streamly.Data.Stream       as S (fold)
import qualified Streamly.Generators        as S (timer, uiBUS, watcher)
import qualified Streamly.Matcher           as S (matcher)
import qualified Transmission.RPC.Client    as TT (runClient)
import           Transmission.RPC.Client    (fromUrl)
import           Types                      (Matcher (Matcher), newClientState,
                                             newState)
import           UI.Client                  (startClient)
import           UI.Constants
import           UI.KeyEvents               (dispatcher, keyConfig)
import           Utils                      (getFileNodes)

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
  nc <- getNumCapabilities
  putStrLn "Generating the list of files in the *arrs folders"
  (ais, aim) <- getFileNodes nc arrPaths
  clientState <- newTVarIO newClientState
  req <- newTChanIO
  pay <- newTChanIO
  prunedVar <- newTVarIO mempty
  ais' <- newTVarIO ais
  aim' <- newTVarIO aim
  itd <- newTVarIO mempty
  tid <- newTVarIO mempty
  ct <- newIORef mempty
  counter <- newIORef 0
  let timerStream = S.timer 5 counter req
      uiStream = S.uiBUS pay req
      watchStream = S.watcher arrPaths
      reservedThreads = 3 -- matcher, client, UI
      wt = max (nc - reservedThreads) 1
      matcher = Matcher wt arrPaths ais' aim' itd tid prunedVar
  vty <- mkVty defaultConfig
  client <- runEff . runWreq . runPrim $ fromUrl url Nothing Nothing
  putStrLn "Starting the transmission client"
  void $ withSimpleTextLogger $ \stl -> runEff .runConcurrent . TT.runClient client . runWreq
    . runPrim . runLog "Client" stl LogTrace . runTime . runFileSystem
    . runClient req clientState chan ct $ startClient
  let appState = newState keyConfig dispatcher clientState pay
  putStrLn "Starting the UI"
  void . forkIO . void $ customMain vty (mkVty defaultConfig) (Just chan) app appState
  putStrLn "Starting the events Stream"
  S.matcher matcher [timerStream, uiStream, watchStream] & S.fold F.drain
