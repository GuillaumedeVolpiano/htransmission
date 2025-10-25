{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Effectful.Timer (
                        startTimer
  )

where

import Brick.BChan (BChan)
import qualified Brick.BChan as BB (writeBChan)
import UI.Types (Events (Tick))
import Effectful (Eff, (:>))
import Control.Monad (forever, void)
import Effectful.Concurrent (Concurrent, forkIO, threadDelay)
import Effectful.Dispatch.Static (unsafeEff_)

writeBChan :: (Concurrent :> es) => BChan a -> a -> Eff es ()
writeBChan chan = unsafeEff_ . BB.writeBChan chan

startTimer :: (Concurrent :> es) => BChan Events -> Eff es ()
startTimer chan = void $ forkIO $ forever $ do
  void $ threadDelay 1000000
  writeBChan chan Tick

