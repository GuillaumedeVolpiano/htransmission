{-# LANGUAGE OverloadedStrings #-}
module UI.Events (
                   appStartEvent
                 , eventHandler
                 , startTimer
                 , updateView
                 , getElements)
where

import           Brick                    (App, BrickEvent (AppEvent, VtyEvent),
                                           EventM, gets, halt, put)
import           Brick.BChan              (BChan, writeBChan)
import           Constants                (basicSession, mainTorrents,
                                           matchedTorrents)
import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Monad            (forever, void)
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Data.Text                (Text)
import           Effectful                (runEff)
import           Effectful.Log            (LogLevel (LogTrace), runLog)
import           Effectful.Prim           (runPrim)
import           Effectful.Reader.Static  (runReader)
import           Effectful.Time           (runTime)
import           Effectful.Wreq           (runWreq)
import           Graphics.Vty             (Event (EvKey), Key (KEsc))
import           Log.Backend.Text         (withSimpleTextLogger)
import           Prelude                  hiding (log)
import           Transmission.RPC.Client  (Client, getSession, getTorrents,
                                           sessionStats)
import           Transmission.RPC.Session (Session, SessionStats)
import           Transmission.RPC.Torrent (Torrent)
import qualified Types                    as T (client, log, view)
import           Types                    (AppState (AppState), Events (..),
                                           View (Main, Prune))

appStartEvent :: App AppState Events ()
appStartEvent = undefined

startTimer :: BChan Events -> IO ()
startTimer chan = void $ forkIO $ forever $ do
  void $ threadDelay 1000000
  writeBChan chan Tick

eventHandler :: BrickEvent n Events -> EventM n AppState ()
eventHandler e = case e of
                     AppEvent Tick            -> updateView
                     VtyEvent (EvKey KEsc []) -> halt
                     _                        -> pure ()

updateView :: EventM n AppState ()
updateView = do
  view <- gets T.view
  client <- gets T.client
  log <- gets T.log
  (getElemsLog, (torrents, sesh, seshStats)) <- liftIO $ getElements view client
  put (AppState view client torrents sesh seshStats (getElemsLog : log))

getElements :: View -> Client -> IO (Text, ([Torrent], Session, SessionStats))
getElements view client = withSimpleTextLogger $ \textLogger -> do
  runEff . runWreq . runPrim . runReader client . runLog "htransmission" textLogger LogTrace . runTime $ do
    let fields = case view of
                Main  -> mainTorrents
                Prune -> matchedTorrents
    torrents <- getTorrents Nothing (Just fields) Nothing
    sesh <- getSession (Just basicSession) Nothing
    seshStats <- sessionStats Nothing
    pure (torrents, sesh, seshStats)
