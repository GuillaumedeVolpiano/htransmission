module Streamly.Generators (timer, uiBUS, watcher)

where

import           Control.Concurrent.STM                 (TChan, atomically,
                                                         newEmptyTMVarIO,
                                                         readTChan, readTMVar,
                                                         writeTChan)
import           Data.IORef                             (IORef, readIORef,
                                                         writeIORef)
import qualified Data.List.NonEmpty                     as NE (fromList)
import qualified Streamly.Data.Stream                   as S (fromList)
import           Streamly.Data.Stream                   (Stream, unfoldrM)
import           Streamly.Data.Stream.Prelude           (constRate,
                                                         parRepeatM)
import           Streamly.Internal.FileSystem.PosixPath (fromString_)
import           Streamly.Internal.FS.Event.Linux       (watchRecursive)
import           Types                                  (RPCPayload (TimerMajor, TimerMinor),
                                                         RPCRequest (RPCRequest),
                                                         UpdateEvent (FSEvent, RPCEvent))
import System.Posix.ByteString (RawFilePath)
import qualified Data.ByteString.Char8 as BS (unpack)

timer :: Int -> IORef Int -> TChan RPCRequest -> Stream IO UpdateEvent
timer period counter rpcIn =  parRepeatM (constRate 2) tick
    where
      tick = do
        c <- readIORef counter
        rpcTimer <- newEmptyTMVarIO
        sel <- if c == 0 then pure TimerMajor else pure TimerMinor
        atomically . writeTChan rpcIn $ RPCRequest sel rpcTimer
        (res, broadcaster) <- atomically . readTMVar $ rpcTimer
        writeIORef counter ((c + 1) `mod` period)
        pure $ RPCEvent (S.fromList res) broadcaster

uiBUS :: TChan RPCPayload -> TChan RPCRequest -> Stream IO UpdateEvent
uiBUS reqChan rpcIn = unfoldrM uievent ()
  where
    uievent _ = do
      ev <- atomically $ readTChan reqChan
      rpcBus <- newEmptyTMVarIO
      atomically . writeTChan rpcIn $ RPCRequest ev rpcBus
      (res, broadcaster) <- atomically $ readTMVar rpcBus
      pure $ Just (RPCEvent (S.fromList res) broadcaster, ())

-- | Start a recursive filesystem watcher on the given list of directory
--   paths.  
--  
--   Each file system event is wrapped as an 'FSEvent' and emitted into the
--   resulting 'Stream'.  
--  
--   /Note:/ Uses Linux inotify via streamly.
watcher :: [RawFilePath] -> Stream IO UpdateEvent
watcher = fmap FSEvent . watchRecursive . NE.fromList . map (fromString_ . BS.unpack)
