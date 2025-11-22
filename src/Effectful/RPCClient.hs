{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Effectful.RPCClient (
                             RPCClient
                           , curView
                           , sortKey
                           , reverseSort
                           , readRPC
                           , notifyUI
                           , writeCurrentTorrents
                           , readCurrentTorrents
                           , runRPCClient
                           , isPrunableReady
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

data RPCClient :: Effect

type instance DispatchOf RPCClient = Static WithSideEffects

data instance StaticRep RPCClient = RPCClient {
                                          rpcIn :: TChan RPCRequest,
                                          clientState :: TVar ClientState,
                                          uiUpdate :: BChan Events,
                                          currentTorrents :: IORef IntSet,
                                          prunableReady :: TVar Bool
                                        }

curView :: (Concurrent :> es, RPCClient :> es) => Eff es View
curView = getStaticRep >>= fmap T.curView . readTVarIO . clientState

sortKey :: (Concurrent :> es, RPCClient :> es) => Eff es Sort
sortKey = getStaticRep >>= fmap T.sortKey . readTVarIO . clientState

reverseSort :: (Concurrent :> es, RPCClient :> es) => Eff es Bool
reverseSort = getStaticRep >>= fmap T.reverseSort . readTVarIO . clientState

readRPC :: (Concurrent :> es, RPCClient :> es) => Eff es RPCRequest
readRPC = getStaticRep >>= atomically . readTChan . rpcIn

notifyUI :: (Concurrent :> es, RPCClient :> es) => Events -> Eff es ()
notifyUI ev = getStaticRep >>= unsafeEff_ . flip writeBChan ev . uiUpdate

readCurrentTorrents :: (Prim :> es, RPCClient :> es) => Eff es IntSet
readCurrentTorrents = getStaticRep >>= readIORef . currentTorrents

writeCurrentTorrents :: (Prim :> es, RPCClient :> es) => IntSet -> Eff es ()
writeCurrentTorrents ct = getStaticRep >>= flip writeIORef ct . currentTorrents

isPrunableReady :: (Concurrent :> es, RPCClient :> es) => Eff es Bool
isPrunableReady = getStaticRep >>= readTVarIO . prunableReady 

runRPCClient :: (IOE :> es) => TChan RPCRequest -> TVar ClientState -> BChan Events -> IORef IntSet 
             -> TVar Bool -> Eff (RPCClient : es) a -> Eff es a
runRPCClient r c u ct pr = evalStaticRep $ RPCClient r c u ct pr
