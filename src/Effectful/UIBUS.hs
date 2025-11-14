{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Effectful.UIBUS (
                          awaitEvent
                        , postReq
                        , readUIState
                        , notifyUI
                        , runUIBUS
                        , UIBUS
                        )
where

import           Brick.BChan               (BChan, writeBChan)
import           Effectful                 (Dispatch (Static), DispatchOf, Eff,
                                            Effect, IOE, (:>))
import           Effectful.Concurrent.STM  (Concurrent, TChan, TVar, atomically,
                                            orElse, readTChan, readTVarIO,
                                            writeTChan)
import           Effectful.Dispatch.Static (SideEffects (WithSideEffects),
                                            StaticRep, evalStaticRep,
                                            getStaticRep, unsafeEff_)
import           Effectful.Timer           (Timer, getTick)
import           Types                     (Req, UpdateEvent (..), ClientState, Events)

data UIBUS :: Effect where

type instance DispatchOf UIBUS = Static WithSideEffects

data instance StaticRep UIBUS =
  UIBUS {
      uiIn :: TVar ClientState
      , uiRequest :: TChan Req
      , uiOut :: BChan Events }

-- | Block until either a UI request or a timer event arrives.
awaitEvent :: (UIBUS :> es, Concurrent :> es, Timer :> es) => Eff es UpdateEvent
awaitEvent = do
  client <- getStaticRep
  tick <- ((\t -> if t then TimerMajor else TimerMinor) <$>) <$> getTick
  atomically $
       (ReqEvent <$> readTChan (uiRequest client)) `orElse` tick

-- | Post a request into the uiRequest channel
postReq :: (UIBUS :> es, Concurrent :> es) => Req -> Eff es ()
postReq req = do
  client <- getStaticRep
  atomically $ writeTChan (uiRequest client) req

-- | Read UI state TVar
readUIState :: (UIBUS :> es, Concurrent :> es) => Eff es ClientState
readUIState = do
  client <- getStaticRep
  readTVarIO (uiIn client)

-- | Notify the Brick UI BChan (this uses unsafeEff_ to call the IO function)
notifyUI :: (UIBUS :> es) => Events -> Eff es ()
notifyUI ev = do
  client <- getStaticRep
  unsafeEff_ $ writeBChan (uiOut client) ev

----------------------------------------------------------------
-- Runner: install the static rep from your existing `UIBUS` record
----------------------------------------------------------------

-- | Run the UIBUS effect by supplying the concrete channels/TVars from your UIBUS
runUIBUS
  :: (IOE :> es) => TVar ClientState
  -> TChan Req      -- ^ uiRequest
  -> BChan Events   -- ^ uiOut
  -> Eff (UIBUS : es) a
  -> Eff es a
runUIBUS uiInV uiReq uiOutV =
  evalStaticRep UIBUS
    { uiIn       = uiInV
    , uiRequest  = uiReq
    , uiOut      = uiOutV
    }
