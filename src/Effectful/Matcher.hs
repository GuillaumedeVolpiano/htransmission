{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Effectful.Matcher (
                          Matcher
                         , matcher
                         , notifyNewUpdate
                         , checkUpdate
                         , readTorrents
                         , writeTorrents
                         , readMatcher
                         , writeMatcher
                         , runMatcher)
where
import           Data.IntSet               (IntSet, fromList)
import           Effectful                 (Dispatch (Static), DispatchOf, Eff,
                                            Effect, IOE, (:>))
import           Effectful.Concurrent.STM  (Concurrent, TChan, TVar, atomically,
                                            readTChan, readTVarIO, writeTChan,
                                            writeTVar)
import           Effectful.Dispatch.Static (SideEffects (WithSideEffects),
                                            StaticRep, evalStaticRep,
                                            getStaticRep)
import           Transmission.RPC.Torrent  (Torrent (toId))
import Effectful.Unix (Unix)
import Control.Monad (void, forever, when)
import Effectful.Concurrent (forkIO)
import Data.Maybe (fromJust)
import Utils (extractPrunable, mkPathMap)
import Constants (arrPaths, pathMap)

data Matcher :: Effect where

type instance DispatchOf Matcher = Static WithSideEffects

data instance StaticRep Matcher =
  Matcher {
            newUpdate :: TChan Bool
          , matcherIn :: TVar [Torrent]
          , matcherOut :: TVar IntSet
          }

-- | Notify the `newUpdate` channel
notifyNewUpdate :: (Matcher :> es, Concurrent :> es) => Bool -> Eff es ()
notifyNewUpdate b = getStaticRep >>= atomically . flip writeTChan b . newUpdate

-- | Check if there has been an update
checkUpdate :: (Matcher :> es, Concurrent :> es) => Eff es Bool
checkUpdate = getStaticRep >>= atomically . readTChan . newUpdate

-- | Read the current known torrents
readTorrents :: (Matcher :> es, Concurrent :> es) => Eff es [Torrent]
readTorrents = getStaticRep >>= readTVarIO . matcherIn

-- | Overwrite the saved torrents
writeTorrents :: (Matcher :> es, Concurrent :> es) => [Torrent] -> Eff es ()
writeTorrents ts = getStaticRep >>= atomically . flip writeTVar ts . matcherIn

-- | Read matcher set
readMatcher :: (Matcher :> es, Concurrent :> es) => Eff es IntSet
readMatcher = getStaticRep >>= readTVarIO . matcherOut

-- | Write matcehr set
writeMatcher :: (Matcher :> es, Concurrent :> es) => IntSet -> Eff es ()
writeMatcher matches = getStaticRep >>= atomically . flip writeTVar matches . matcherOut

runMatcher :: (IOE :> es) => TChan Bool -> TVar [Torrent] -> TVar IntSet -> Eff (Matcher : es) a -> Eff es a
runMatcher nu mi = evalStaticRep . Matcher nu mi

matcher :: (Unix :> es, Concurrent :> es, Matcher :> es) => Eff es ()
matcher = void . forkIO $ forever $ do
  u <- checkUpdate
  when u $ do
    torrents <- readTorrents
    matched <- fromList . map (fromJust . toId)
      <$> extractPrunable arrPaths (mkPathMap pathMap) torrents
    writeMatcher matched
