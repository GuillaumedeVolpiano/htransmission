{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module UI.Client (
                        startClient)
where
import           Constants                (basicSession, mainTorrents)
import           Control.Monad            (forever, void, when)
import           Data.Maybe               (fromJust)
import qualified Data.Text                as T (pack)
import           Effectful                (Eff, (:>))
import           Effectful.Concurrent     (Concurrent, forkIO)
import           Effectful.FileSystem     (FileSystem)
import           Effectful.Log            (Log, logInfo_)
import           Effectful.Matcher        (Matcher, matcher, notifyNewUpdate,
                                           readMatcher, readTorrents,
                                           writeTorrents)
import           Effectful.Prim           (Prim)
import           Effectful.Reader.Static  (Reader)
import           Effectful.Time           (Time)
import           Effectful.Timer          (Timer, tick)
import           Effectful.UIBUS          (UIBUS, awaitEvent, notifyUI,
                                           readUIState)
import           Effectful.Unix           (Unix)
import           Effectful.Wreq           (Wreq)
import           Transmission.RPC.Client  (addTorrent, deleteTorrent,
                                           getSession, getTorrents,
                                           sessionStats)
import           Transmission.RPC.Session (Session, SessionStats)
import           Transmission.RPC.Torrent
import qualified Transmission.RPC.Types   as TT (Client)
import           Transmission.RPC.Types   (ID (ID), IDs (IDs), Label,
                                           TorrentRef (Path))
import qualified Types                    as UT (reverseSort, sortKey)
import           Types                    (Events (Updated), Req (..),
                                           UpdateEvent (..), curView)
import           UI.Utils                 (sel)

startClient :: (Reader TT.Client :> es, UIBUS :> es, Concurrent :> es, Wreq :> es, Prim :> es, Log :> es, Time :> es, Unix :> es, FileSystem :> es, Timer :> es, Matcher :> es)
            => Eff es ()
startClient = do
  tick
  matcher
  void . forkIO $ forever $ do
    event <- awaitEvent
    torrents <- readTorrents
    clientState <- readUIState
    case event of
      (ReqEvent req) -> case req of
          Get -> pure ()
          Delete tors@(torIds, deleteData) -> do
            logInfo_ (T.pack ("Deleting torrents " ++ show tors))
            deleteTorrent (IDs . map ID $ torIds) deleteData Nothing
          Add tors -> addElements tors
      _ -> pure ()
    let isMajor = case event of
                    TimerMajor -> True
                    _          -> False
    (torrents', sesh, seshStats) <- getElements isMajor torrents
    when isMajor $ do
      notifyNewUpdate isMajor
      writeTorrents torrents'
    unmatched <- readMatcher
    let view = curView clientState
        sortKey = UT.sortKey clientState
        reverseSort = UT.reverseSort clientState
        torrents'' = sel view unmatched sortKey reverseSort torrents'
    notifyUI (Updated torrents'' sesh seshStats)

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


