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
import qualified Data.Text as T (pack)
import           Effectful                 (Eff, (:>))
import           Effectful.Concurrent      (Concurrent, forkIO)
import           Effectful.Concurrent.STM  (STM, TVar, atomically, readTVar,
                                            writeTVar)
import           Effectful.Dispatch.Static (unsafeEff_)
import           Effectful.FileSystem      (FileSystem)
import           Effectful.Log             (Log, logInfo_)
import           Effectful.Prim            (Prim)
import           Effectful.Reader.Static   (Reader)
import           Effectful.Time            (Time)
import           Effectful.Unix            (Unix)
import           Effectful.Wreq            (Wreq)
import           Transmission.RPC.Client   (addTorrent, deleteTorrent,
                                            getSession, getTorrents,
                                            sessionStats)
import           Transmission.RPC.Session  (Session, SessionStats)
import           Transmission.RPC.Torrent
import           Transmission.RPC.Types    (Client, ID (ID), IDs (IDs), Label,
                                            TorrentRef (Path))
import           Types                     (Action (Matched), FIFOSet, Req (..),
                                            dequeue, enqueue)
import           UI.Types                  (Events (Updated), View (Prune))
import           UI.Utils                  (actionFromView)
import           Utils                     (extractPrunable, mkPathMap)

writeBChan :: Concurrent :> es => BChan a -> a -> Eff es ()
writeBChan chan = unsafeEff_ . BB.writeBChan chan

enqueueShared :: Ord a => a -> TVar (FIFOSet a) -> STM ()
enqueueShared x fifoVar = do
  fifoSet <- readTVar fifoVar
  let fifoSet' = enqueue x fifoSet
  writeTVar fifoVar fifoSet'

dequeueShared :: Ord a => TVar (FIFOSet a) -> STM  (Maybe a)
dequeueShared fifoVar = do
  fifoSet <- readTVar fifoVar
  case dequeue fifoSet of
    Nothing -> pure Nothing
    Just (x, fifoSet') -> do
      writeTVar fifoVar fifoSet'
      pure (Just x)

startClient :: (Reader Client :> es, Concurrent :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es, Unix :> es, FileSystem :> es)
            => TVar (FIFOSet (Bool, View, Req)) -> BChan Events -> Eff es ()
startClient fifoVar bChan = void . forkIO $ forever $ do
  maybeView <- atomically $ dequeueShared fifoVar
  case maybeView of
    Nothing -> pure ()
    (Just (isSwitch, view, req)) -> do
      case req of
        Get -> pure ()
        Delete tors@(torIds, deleteData) -> do
          logInfo_ (T.pack ("Deleting torrents " ++ show tors)) 
          deleteTorrent (IDs . map ID $ torIds) deleteData Nothing
          (torrents, sesh, seshStats) <- getElements . actionFromView $  view
          writeBChan bChan (Updated isSwitch view torrents sesh seshStats)
        Add tors -> do
          addElements tors
      (torrents, sesh, seshStats) <- getElements . actionFromView $ view
      torrents' <- treatTorrents view torrents
      writeBChan bChan (Updated isSwitch view torrents' sesh seshStats)

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

addElements :: (Reader Client :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es, Unix :> es, FileSystem :> es) =>
  [(FilePath, FilePath, [Label])]-> Eff es ()
addElements torIds = do
  mapM_ ((\(t, d, l) -> addTorrent t Nothing d Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing l Nothing) . (\(a, b, c) -> (Path a, Just b, Just c))) torIds

treatTorrents :: (Unix :> es) => View -> [Torrent] -> Eff es [Torrent]
treatTorrents Prune = extractPrunable arrPaths (mkPathMap pathMap)
treatTorrents _ = pure

