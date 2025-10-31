{-# LANGUAGE GADTs #-}
module Effectful.Types (
                         Matcher
                       , inVar
                       , updated
                       , outVar
                       , newMatcher
                       , Client
                       , uiIn
                       , uiOut
                       , matcherIn
                       , matcherOut
                       , newClient
                       , newUpdate
                       , Request
  )

where
import Transmission.RPC.Torrent (Torrent)
import Effectful.Concurrent.STM (TVar, TChan)
import Data.IntSet (IntSet)
import UI.Types (Events, Request)
import Brick.BChan (BChan)

data Matcher where
  Matcher :: {updated :: TChan Bool, inVar :: TVar [Torrent], outVar :: TVar IntSet} ->
               Matcher

data Client where
  Client :: {
        newUpdate :: TChan Bool
      , matcherOut :: TVar [Torrent]
      , matcherIn :: TVar IntSet
      , uiIn :: TVar Request
      , uiOut :: BChan Events } -> Client

newMatcher :: TChan Bool -> TVar [Torrent] -> TVar IntSet -> Matcher
newMatcher = Matcher

newClient :: TChan Bool -> TVar [Torrent] -> TVar IntSet -> TVar Request -> BChan Events -> Client
newClient = Client
