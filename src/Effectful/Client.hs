{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Effectful.Client (
                          Client
                        , curView
                        , sortKey
                        , reverseSort
                        , readRPC
                        , notifyUI
                        , writeCurrentTorrents
                        , readCurrentTorrents
                        , runClient
                        )

where

import           Brick.BChan               (BChan, writeBChan)
import           Data.IntSet               (IntSet)
import           Effectful                 (Dispatch (Static), DispatchOf, Eff,
                                            Effect, IOE, (:>))
import           Effectful.Concurrent.STM  (Concurrent, TChan, TVar, atomically,
                                            readTChan, readTVarIO)
import           Effectful.Dispatch.Static (SideEffects (WithSideEffects),
                                            StaticRep, evalStaticRep,
                                            getStaticRep, unsafeEff_)
import           Effectful.Prim.IORef      (IORef, Prim, readIORef, writeIORef)
import qualified Types                     as T (curView, reverseSort, sortKey)
import           Types                     (ClientState, Events, RPCRequest,
                                            Sort, View)

data Client :: Effect

type instance DispatchOf Client = Static WithSideEffects

data instance StaticRep Client = Client {
                                          rpcIn :: TChan RPCRequest,
                                          clientState :: TVar ClientState,
                                          uiUpdate :: BChan Events,
                                          currentTorrents :: IORef IntSet
                                        }

curView :: (Concurrent :> es, Client :> es) => Eff es View
curView = getStaticRep >>= fmap T.curView . readTVarIO . clientState

sortKey :: (Concurrent :> es, Client :> es) => Eff es Sort
sortKey = getStaticRep >>= fmap T.sortKey . readTVarIO . clientState

reverseSort :: (Concurrent :> es, Client :> es) => Eff es Bool
reverseSort = getStaticRep >>= fmap T.reverseSort . readTVarIO . clientState

readRPC :: (Concurrent :> es, Client :> es) => Eff es RPCRequest
readRPC = getStaticRep >>= atomically . readTChan . rpcIn

notifyUI :: (Concurrent :> es, Client :> es) => Events -> Eff es ()
notifyUI ev = getStaticRep >>= unsafeEff_ . flip writeBChan ev . uiUpdate

readCurrentTorrents :: (Prim :> es, Client :> es) => Eff es IntSet
readCurrentTorrents = getStaticRep >>= readIORef . currentTorrents

writeCurrentTorrents :: (Prim :> es, Client :> es) => IntSet -> Eff es ()
writeCurrentTorrents ct = getStaticRep >>= flip writeIORef ct . currentTorrents

runClient :: (IOE :> es) => TChan RPCRequest -> TVar ClientState -> BChan Events -> IORef IntSet -> Eff (Client : es) a -> Eff es a
runClient r c u ct = evalStaticRep $ Client r c u ct
