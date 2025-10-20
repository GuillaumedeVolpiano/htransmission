{-# LANGUAGE OverloadedStrings #-}
module UI.Events (
                   appStartEvent
                 , eventHandler
                 , startTimer
                 , updateView
                 , getElements
                 , switchView)
where

import           Brick                    (App, BrickEvent (AppEvent, VtyEvent),
                                           EventM, gets, modify)
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
import           Graphics.Vty             (Event (EvKey))
import           Log.Backend.Text         (withSimpleTextLogger)
import           Prelude                  hiding (log)
import           Transmission.RPC.Client  (Client, getSession, getTorrents,
                                           sessionStats)
import           Transmission.RPC.Session (Session, SessionStats)
import           Transmission.RPC.Torrent (Torrent)
import qualified Types                    as T (client, log, view, torrents, session, sessionStats, keyHandler)
import           Types                    (Events (..),
                                           View (..), AppState)
import Brick.Keybindings (handleKey)

appStartEvent :: App AppState Events ()
appStartEvent = undefined

startTimer :: BChan Events -> IO ()
startTimer chan = void $ forkIO $ forever $ do
  void $ threadDelay 1000000
  writeBChan chan Tick

eventHandler :: BrickEvent Int Events -> EventM Int AppState ()
eventHandler (AppEvent Tick) = updateView
eventHandler (VtyEvent (EvKey k mods)) = gets T.keyHandler >>= \kh -> void (handleKey kh k mods)


eventHandler _ = pure ()

updateView :: EventM n AppState ()
updateView = do
  view <- gets T.view
  client <- gets T.client
  (getElemsLog, (torrents, sesh, seshStats)) <- liftIO $ getElements view client
  modify (\a -> a{T.torrents = torrents, T.session=sesh, T.sessionStats = seshStats, T.log = getElemsLog : T.log a})

getElements :: View -> Client -> IO (Text, ([Torrent], Session, SessionStats))
getElements view client = withSimpleTextLogger $ \textLogger -> do
  runEff . runWreq . runPrim . runReader client . runLog "htransmission" textLogger LogTrace . runTime $ do
    let fields = case view of
                Prune -> matchedTorrents
                _     -> mainTorrents
    torrents <- getTorrents Nothing (Just fields) Nothing
    sesh <- getSession (Just basicSession) Nothing
    seshStats <- sessionStats Nothing
    pure (torrents, sesh, seshStats)

switchView :: View -> EventM n AppState ()
switchView view = modify (\a -> a{T.view = view})
