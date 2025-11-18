module Streamly.UIBUS (uiBUS)

where

import           Control.Concurrent.STM (TChan, atomically, readTChan,
                                         writeTChan, newEmptyTMVarIO, readTMVar)
import           Streamly.Data.Stream   (Stream, unfoldrM)
import qualified Streamly.Data.Stream as S (fromList)
import           Streamly.Data.Stream.Prelude (parConcat)
import           Types                  (RPCPayload, RPCRequest (RPCRequest), UpdateEvent (RPCEvent), Response (End, T))

uiBUS :: TChan RPCPayload -> TChan RPCRequest -> Stream IO UpdateEvent
uiBUS reqChan rpcIn = parConcat id $ unfoldrM uievent ()
  where
    uievent _ = do
      ev <- atomically $ readTChan reqChan
      rpcBus <- newEmptyTMVarIO
      atomically . writeTChan rpcIn $ RPCRequest ev rpcBus
      (res, broadcaster) <- atomically $ readTMVar rpcBus
      let ret = map (RPCEvent . T) res ++ [RPCEvent . End $ broadcaster]
      pure $ Just (S.fromList ret, ())
