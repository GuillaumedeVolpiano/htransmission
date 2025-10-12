{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Effectful.Brick (
  -- * Effect
  Brick
  -- ** Handlers
  , runBrick
  -- * App
  , simpleMain
  -- reexports
  , Widget
  , emptyWidget
  , fill
  , hLimit
  , hLimitPercent
  , Padding (..)
  , padLeft
  , padRight
  , str
  , txt
  , vLimit
  , vLimitPercent
  , (<=>)
  , (<+>)
  , withBorderStyle
  )
where
import           Brick                     (Padding (..), Widget, emptyWidget,
                                            fill, hLimit, hLimitPercent,
                                            padLeft, padRight, str, txt, vLimit,
                                            vLimitPercent, withBorderStyle,
                                            (<+>), (<=>))
import qualified Brick                     as B (simpleMain)
import           Effectful                 (Dispatch (Static), DispatchOf, Eff,
                                            Effect, IOE, (:>))
import           Effectful.Dispatch.Static (HasCallStack,
                                            SideEffects (WithSideEffects),
                                            StaticRep, evalStaticRep,
                                            unsafeEff_)

data Brick :: Effect where

type instance DispatchOf Brick = Static WithSideEffects
data instance StaticRep Brick = Brick

runBrick :: (HasCallStack, IOE :> es) => Eff (Brick : es) a -> Eff es a
runBrick = evalStaticRep Brick

simpleMain :: (Brick :> es, Ord n) => Widget n -> Eff es ()
simpleMain = unsafeEff_ . B.simpleMain
