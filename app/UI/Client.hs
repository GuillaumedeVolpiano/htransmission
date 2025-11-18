{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module UI.Client (
                        startClient)
where
import           Constants                (basicSession, mainTorrents)
import           Control.Monad            (forever, void)
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IS (fromList, toList)
import           Data.Maybe               (fromJust)
import qualified Data.Text                as T (pack)
import           Effectful                (Eff, (:>))
import           Effectful.Client         (Client, notifyUI,
                                           readCurrentTorrents, readRPC,
                                           writeCurrentTorrents)
import qualified Effectful.Client         as C (curView, reverseSort, sortKey)
import           Effectful.Concurrent     (Concurrent, forkIO)
import           Effectful.Concurrent.STM (atomically, newEmptyTMVarIO,
                                           putTMVar, readTMVar)
import           Effectful.FileSystem     (FileSystem)
import           Effectful.Log            (Log, logInfo_)
import           Effectful.Prim           (Prim)
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

startClient :: (TT.Client :> es, Concurrent :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es, FileSystem :> es, Client :> es)
            => Eff es ()
startClient = do
  void . forkIO . forever $ do
    req <- readRPC
    let outChan = chan req
    tl <- case payload req of
            TimerMajor -> pure Nothing
            TimerMinor -> Just <$> readCurrentTorrents
            Get torList -> pure torList
            Delete tors@(torIds, deleteData) -> do
              logInfo_ (T.pack ("Deleting torrents " ++ show tors))
              deleteTorrent (IDs . map ID . IS.toList $ torIds) deleteData Nothing
              pure Nothing
            Add tors -> addElements tors >> pure Nothing
    (torrents', sesh, seshStats) <- getElements tl
    broadcaster <- newEmptyTMVarIO
    atomically $ do
      putTMVar outChan (torrents', broadcaster)
    unmatched <- atomically $ do
      readTMVar broadcaster
    view <- C.curView
    sortKey <- C.sortKey
    reverseSort <- C.reverseSort
    let torrents'' = sel view unmatched sortKey reverseSort torrents'
    writeCurrentTorrents . IS.fromList . map (fromJust . toId) $ torrents''
    notifyUI (Types.Updated torrents'' sesh seshStats)

getElements :: (TT.Client :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es) =>
    Maybe IntSet -> Eff es ([Torrent], Session, SessionStats)
getElements torrents = do
    let tids = IDs . map ID . IS.toList <$> torrents
    torrents' <- getTorrents tids (Just mainTorrents) Nothing
    sesh <- getSession (Just basicSession) Nothing
    seshStats <- sessionStats Nothing
    pure (torrents', sesh, seshStats)

addElements :: (TT.Client :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es, FileSystem :> es) =>
  [(FilePath, FilePath, [Label])]-> Eff es ()
addElements torIds = do
  mapM_ ((\(t, d, l) -> addTorrent t Nothing d Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing l Nothing) . (\(a, b, c) -> (Path a, Just b, Just c))) torIds
