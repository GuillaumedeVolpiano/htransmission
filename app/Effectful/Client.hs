{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Effectful.Client (
                          enqueueShared
                        , startClient)
where
import           Brick.BChan               (BChan)
import qualified Brick.BChan               as BB (writeBChan)
import           Constants                 (arrPaths, basicSession,
                                            mainTorrents, matchedTorrents,
                                            pathMap)
import           Control.Monad             (forever, void)
import           Effectful                 (Eff, (:>))
import           Effectful.Concurrent      (Concurrent, forkIO)
import           Effectful.Concurrent.STM  (STM, TVar, atomically, readTVar,
                                            writeTVar)
import           Effectful.Dispatch.Static (unsafeEff_)
import           Effectful.Log             (Log)
import           Effectful.Prim            (Prim)
import           Effectful.Reader.Static   (Reader)
import           Effectful.Time            (Time)
import           Effectful.Unix            (Unix)
import           Effectful.Wreq            (Wreq)
import           Transmission.RPC.Client   (getSession, getTorrents,
                                            sessionStats)
import           Transmission.RPC.Session  (Session, SessionStats)
import           Transmission.RPC.Torrent
import           Transmission.RPC.Types    (Client)
import           Types                     (Action (Matched), FIFOSet, dequeue,
                                            enqueue)
import           UI.Types                  (Events (Updated), View)
import           UI.Utils                  (actionFromView)
import           Utils                     (extractPrunable, mkPathMap)

writeBChan :: Concurrent :> es => BChan a -> a -> Eff es ()
writeBChan chan = unsafeEff_ . BB.writeBChan chan

enqueueShared :: Ord a => a -> TVar (FIFOSet a) -> STM ()
enqueueShared x fifoVar = do
  fifoSet <- readTVar fifoVar
  let fifoSet' = enqueue x fifoSet
  writeTVar fifoVar fifoSet'

dequeueShared :: Ord a => TVar (FIFOSet a) -> STM  (Maybe a)
dequeueShared fifoVar = do
  fifoSet <- readTVar fifoVar
  case dequeue fifoSet of
    Nothing -> pure Nothing
    Just (x, fifoSet') -> do
      writeTVar fifoVar fifoSet'
      pure (Just x)

startClient :: (Reader Client :> es, Concurrent :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es, Unix :> es)
            => TVar (FIFOSet (Bool, View)) -> BChan Events -> Eff es ()
startClient fifoVar bChan = void . forkIO $ forever $ do
  maybeView <- atomically $ dequeueShared fifoVar
  case maybeView of
    Nothing -> pure ()
    (Just (isSwitch, view)) -> do
      (torrents, sesh, seshStats) <- getElements . actionFromView $ view
      writeBChan bChan (Updated isSwitch view torrents sesh seshStats)


getElements :: (Reader Client :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es, Unix :> es) => Action
            -> Eff es ([Torrent], Session, SessionStats)
getElements action = do
    let fields = case action of
                Matched -> matchedTorrents
                _       -> mainTorrents
    torrents <- case action of
                  Matched -> getTorrents Nothing (Just fields) Nothing
                    >>= extractPrunable arrPaths (mkPathMap pathMap)
                  _ -> getTorrents Nothing (Just fields) Nothing
    sesh <- getSession (Just basicSession) Nothing
    seshStats <- sessionStats Nothing
    pure (torrents, sesh, seshStats)
