{-# LANGUAGE GADTs #-}
module Effectful.Types (
                         Matcher
                       , inVar
                       , updated
                       , outVar
                       , newMatcher
                       , Client
                       , uiIn
                       , uiRequest
                       , uiOut
                       , timerIn
                       , matcherIn
                       , matcherOut
                       , newClient
                       , newUpdate
  )

where
import Transmission.RPC.Torrent (Torrent)
import Effectful.Concurrent.STM (TVar, TChan)
import Data.IntSet (IntSet)
import UI.Types (Events, ClientState)
import Brick.BChan (BChan)
import Types (Req)

data Matcher where
  Matcher :: {updated :: TChan Bool, inVar :: TVar [Torrent], outVar :: TVar IntSet} ->
               Matcher

data Client where
  Client :: {
        newUpdate :: TChan Bool
      , matcherOut :: TVar [Torrent]
      , matcherIn :: TVar IntSet
      , timerIn  :: TChan Bool
      , uiIn :: TVar ClientState
      , uiRequest :: TChan Req 
      , uiOut :: BChan Events } -> Client

newMatcher :: TChan Bool -> TVar [Torrent] -> TVar IntSet -> Matcher
newMatcher = Matcher

newClient :: TChan Bool -> TVar [Torrent] -> TVar IntSet -> TChan Bool -> TVar ClientState -> TChan Req 
          -> BChan Events -> Client
newClient = Client
