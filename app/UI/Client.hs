{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module UI.Client (
                   startClient
                 )
where
import           Constants                (basicSession, mainTorrents)
import           Control.Monad            (forever, void, forM_)
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IS (fromList, toList)
import           Data.Maybe               (fromJust)
import qualified Data.Text                as T (pack)
import           Effectful                (Eff, (:>))
import           Effectful.Concurrent     (Concurrent, forkIO)
import           Effectful.Concurrent.STM (atomically, newEmptyTMVarIO,
                                           putTMVar, readTMVar)
import           Effectful.FileSystem     (FileSystem)
import           Effectful.Log            (Log, logInfo_, logTrace_)
import           Effectful.Prim           (Prim)
import           Effectful.RPCClient      (RPCClient, notifyUI,
                                           readCurrentTorrents, readRPC,
                                           writeCurrentTorrents, isPrunableReady)
import qualified Effectful.RPCClient      as C (curView, reverseSort, sortKey)
import           Effectful.Time           (Time)
import           Effectful.Wreq           (Wreq)
import qualified Transmission.RPC.Client  as TT (Client)
import           Transmission.RPC.Client  (addTorrent, deleteTorrent,
                                           getSession, getTorrents,
                                           sessionStats)
import           Transmission.RPC.Session (Session, SessionStats)
import           Transmission.RPC.Torrent (Torrent, toId)
import           Transmission.RPC.Types   (ID (ID), IDs (IDs), Label,
                                           TorrentRef (Path))
import           Types                    (Events (Updated), RPCPayload (..),
                                           RPCRequest (..))
import           UI.Utils                 (sel)

startClient :: (TT.Client :> es, Concurrent :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es, FileSystem :> es, RPCClient :> es)
            => Eff es ()
startClient = do
  void . forkIO . forever $ do
    logTrace_ "Waiting for a request"
    req <- readRPC
    logTrace_ "Got request"
    let outChan = chan req
    tl <- case payload req of
            TimerMajor -> pure Nothing
            TimerMinor -> Just <$> readCurrentTorrents
            Get torList -> pure torList
            Delete tors@(torIds, deleteData) -> do
              logInfo_ (T.pack ("Deleting torrents " ++ show tors))
              deleteTorrent (IDs . map ID . IS.toList $ torIds) deleteData Nothing
              pure Nothing
            Add fps downloadDir labels _ started  -> addElements fps downloadDir labels started
              >> pure Nothing
    (torrents', sesh, seshStats) <- getElements tl
    logTrace_ "Got request results, sending to the matcher"
    broadcaster <- newEmptyTMVarIO
    atomically $ do
      putTMVar outChan (torrents', broadcaster)
    pr <- isPrunableReady
    unmatched <- if pr then do
      logTrace_ "Waiting for matcher response"
      u <- fmap Just .  atomically $ readTMVar broadcaster
      logTrace_ "Got matcher response"
      pure u
                                    else pure Nothing
    view <- C.curView
    sortKey <- C.sortKey
    reverseSort <- C.reverseSort
    let torrents'' = sel view unmatched sortKey reverseSort torrents'
    writeCurrentTorrents . IS.fromList . map (fromJust . toId) $ torrents''
    notifyUI (Updated torrents' torrents'' sesh seshStats unmatched)

getElements :: (TT.Client :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es) =>
    Maybe IntSet -> Eff es ([Torrent], Session, SessionStats)
getElements torrents = do
    let tids = IDs . map ID . IS.toList <$> torrents
    torrents' <- getTorrents tids (Just mainTorrents) Nothing
    sesh <- getSession (Just basicSession) Nothing
    seshStats <- sessionStats Nothing
    pure (torrents', sesh, seshStats)

addElements :: (TT.Client :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es, FileSystem :> es) =>
  [FilePath]-> FilePath -> [Label] -> Bool -> Eff es ()
addElements fps downloadDir labels started = do
  forM_ fps $ \t -> addTorrent (Path t) Nothing (Just downloadDir) Nothing Nothing (Just . not $ started) Nothing
    Nothing Nothing Nothing Nothing (Just labels) Nothing
