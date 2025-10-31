{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Effectful.Client (
                          enqueueShared
                        , startClient)
where
import           Brick.BChan               (BChan)
import qualified Brick.BChan               as BB (writeBChan)
import           Constants                 (basicSession,
                                            mainTorrents)
import           Control.Monad             (forever, void, when)
import qualified Data.Text as T (pack)
import           Effectful                 (Eff, (:>))
import           Effectful.Concurrent      (Concurrent, forkIO)
import           Effectful.Concurrent.STM  (STM, TVar, atomically, readTVar,
                                            writeTVar, writeTChan, readTVarIO)
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
import qualified Transmission.RPC.Types as TT (Client)
import           Transmission.RPC.Types    (ID (ID), IDs (IDs), Label,
                                            TorrentRef (Path))
import           Types                     (FIFOSet, Req (..),
                                            dequeue, enqueue)
import           UI.Types                  (Events (Updated))
import           UI.Utils                  (sel)
import Effectful.Types (Client, uiIn, uiOut, matcherIn, matcherOut, newUpdate)
import Data.Maybe (fromJust)

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

startClient :: (Reader TT.Client :> es, Concurrent :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es, Unix :> es, FileSystem :> es)
            => Client -> Eff es ()
startClient client = void . forkIO $ forever $ do
  let fifoVar = uiIn client
      bChan = uiOut client
      setVar = matcherIn client
      tVar = matcherOut client
      tChan = newUpdate client
  torrents <- readTVarIO tVar
  maybeView <- atomically $ dequeueShared fifoVar
  case maybeView of
    Nothing -> pure ()
    (Just (isSwitch, view, req, sortKey, reverseSort, isMajor)) -> do
      case req of
        Get -> pure ()
        Delete tors@(torIds, deleteData) -> do
          logInfo_ (T.pack ("Deleting torrents " ++ show tors)) 
          deleteTorrent (IDs . map ID $ torIds) deleteData Nothing
        Add tors -> addElements tors
      (torrents', sesh, seshStats) <- getElements isMajor torrents
      when isMajor $ do 
        atomically $ writeTChan tChan isMajor
        atomically $ writeTVar tVar torrents'
      unmatched <- readTVarIO setVar 
      let torrents'' = sel view unmatched sortKey reverseSort torrents'
      writeBChan bChan (Updated isSwitch view torrents'' sesh seshStats)

getElements :: (Reader TT.Client :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es, Unix :> es) =>
    Bool -> [Torrent] -> Eff es ([Torrent], Session, SessionStats)
getElements isMajor torrents = do
    let tids = if isMajor then Nothing else Just . IDs  . map (ID . fromJust . toId) $ torrents 
    torrents' <- getTorrents tids (Just mainTorrents) Nothing
    sesh <- getSession (Just basicSession) Nothing
    seshStats <- sessionStats Nothing
    pure (torrents', sesh, seshStats)

addElements :: (Reader TT.Client :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es, Unix :> es, FileSystem :> es) =>
  [(FilePath, FilePath, [Label])]-> Eff es ()
addElements torIds = do
  mapM_ ((\(t, d, l) -> addTorrent t Nothing d Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing l Nothing) . (\(a, b, c) -> (Path a, Just b, Just c))) torIds


