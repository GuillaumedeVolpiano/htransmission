{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Effectful.Timer (
                Timer
              , emptyTimer
              , postTimer
              , runTimer
              , tick
              , getTick)

where

import           Control.Monad             (forever, void, when)
import           Effectful                 (Dispatch (Static), DispatchOf, Eff,
                                            Effect, IOE, (:>))
import           Effectful.Concurrent      (Concurrent, forkIO, threadDelay)
import           Effectful.Concurrent.STM  (STM, TChan, atomically,
                                            isEmptyTChan, readTChan, writeTChan)
import           Effectful.Dispatch.Static (SideEffects (WithSideEffects),
                                            StaticRep, evalStaticRep,
                                            getStaticRep)
import           Effectful.Prim.IORef      (Prim, newIORef, readIORef,
                                            writeIORef)

data Timer :: Effect where

type instance DispatchOf Timer = Static WithSideEffects

data instance StaticRep Timer =
  Timer {
          timer :: TChan Bool
        , period :: Int
        , frequency :: Int
        }

-- | Post a timer event (True = major, False = minor)
postTimer :: (Timer :> es, Concurrent :> es) => Bool -> Eff es ()
postTimer b = getStaticRep >>= atomically . flip writeTChan b . timer

emptyTimer :: (Timer :> es, Concurrent :> es) => Eff es Bool
emptyTimer = getStaticRep >>= atomically . isEmptyTChan . timer

getTick :: (Timer :> es, Concurrent :> es) => Eff es (STM Bool)
getTick = readTChan . timer <$> getStaticRep

runTimer :: (IOE :> es) => TChan Bool -> Int -> Int -> Eff (Timer : es) a -> Eff es a
runTimer chan p = evalStaticRep . Timer chan p

tick :: (Prim :> es, Concurrent :> es, Timer :> es) => Eff es ()
tick = void $ do
  tickCounterRef <- newIORef (0 :: Int)
  t <- getStaticRep
  forkIO $ forever $ do
    void . threadDelay . frequency $ t
    counter <- readIORef tickCounterRef
    isEmpty <- emptyTimer
    when (isEmpty || counter ==0) $ do
      postTimer (counter == 0)
    writeIORef tickCounterRef ((counter + 1) `mod` period t)

