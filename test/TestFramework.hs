{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module TestFramework where

import Rules

import Control.Monad.State
import Control.Monad.Except
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Generators
import Text.Show.Pretty
import Data.Maybe

type UniversePropertyMonad = PropertyM (State (Either String Universe))

universeProperty :: UniversePropertyMonad a -> ArbitraryUniverse -> Property
universeProperty action (ArbitraryUniverse universe) = monadic extractProperty action
  where extractProperty act = checkResult $ runState act (Right universe)
        checkResult (prop, Right resultUniverse) =
          counterexample ("Resulting universe: " ++ ppShow resultUniverse) $
          prop
        checkResult (prop, Left msg) =
          counterexample ("Unexpected error: " ++ msg) $
          prop

getUniverse :: UniversePropertyMonad Universe
getUniverse = do
  universeOrError <- run get
  case universeOrError of
    Right universe -> return universe
    Left msg -> stop $ counterexample ("Tried to access universe, but was already failed: " ++ msg) False

getsUniverse :: (Universe -> a) -> UniversePropertyMonad a
getsUniverse x = x <$> getUniverse

applyToUniverse :: (forall m. MonadError String m => Universe -> m Universe) -> UniversePropertyMonad ()
applyToUniverse action = do
  currentState <- run get
  let nextState = action =<< currentState
  run $ put nextState

shouldHaveFailed :: UniversePropertyMonad ()
shouldHaveFailed = do
  universeOrError <- run get
  case universeOrError of
    Right universe -> stop $ counterexample ("Was expecting failure, but was successful: " ++ ppShow universe) False
    Left _ -> return ()

preMaybe :: Monad m => Maybe b -> PropertyM m b
preMaybe value = pre (isJust value) >> return (fromJust value)

