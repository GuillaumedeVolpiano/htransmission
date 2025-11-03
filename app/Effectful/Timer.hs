{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Effectful.Timer (
                        startTimer
  )

where

import           Control.Monad            (forever, void, when)
import           Effectful                (Eff, (:>))
import           Effectful.Concurrent     (Concurrent, forkIO, threadDelay)
import           Effectful.Concurrent.STM (TChan, atomically, isEmptyTChan,
                                           unGetTChan)
import           Effectful.Prim.IORef     (Prim, newIORef, readIORef,
                                           writeIORef)

startTimer :: (Prim :> es, Concurrent :> es) => TChan Bool -> Eff es ()
startTimer chan = void $ do
  tickCounterRef <- newIORef (0 :: Int)
  forkIO $ forever $ do
    void $ threadDelay 1000000
    counter <- readIORef tickCounterRef
    isEmpty <- atomically $ isEmptyTChan chan
    when (isEmpty || counter == 0) $ do
      atomically . unGetTChan chan $ (counter == 0)
    writeIORef tickCounterRef ((counter + 1) `mod` 5)
