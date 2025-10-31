{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Effectful.Matcher (
                           startMatcher
  )

where
import           Constants                (arrPaths, pathMap)
import           Control.Monad            (forever, void, when)
import           Data.IntSet              (fromList)
import           Data.Maybe               (fromJust)
import           Effectful                (Eff, (:>))
import           Effectful.Concurrent     (Concurrent, forkIO)
import           Effectful.Concurrent.STM (atomically, readTChan, writeTVar, readTVarIO)
import           Effectful.Types          (Matcher)
import qualified Effectful.Types          as ET (inVar, outVar, updated)
import           Effectful.Unix           (Unix)
import           Transmission.RPC.Torrent (toId)
import           Utils                    (extractPrunable, mkPathMap)

startMatcher :: (Unix :> es, Concurrent :> es) => Matcher -> Eff es ()
startMatcher matcher = void . forkIO $ forever $ do
  let inVar = ET.inVar matcher
      outVar = ET.outVar matcher
      updated = ET.updated matcher
  u <- atomically $ readTChan updated
  when u $ do
    torrents <- readTVarIO inVar
    unmatched <- writeTVar outVar . fromList . map (fromJust . toId)
      <$> extractPrunable arrPaths (mkPathMap pathMap) torrents
    atomically unmatched
