module Interaction.ArmingProperties where

import Rules
import TestFramework
import TestHelpers

import Test.Tasty.QuickCheck
import Test.Tasty
import Test.QuickCheck.Monadic
import Data.Maybe
import Data.Function ((&))

armingTests :: TestTree
armingTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Arming tests" [
  testProperty "Cannot arm with more iron than had" $ armingProperty $ do
      plId <- findArmingPlayer
      ironAmount <- fmap getIronAmount $ getsUniverse getPlayerResources <*> pure plId
      armAmount <- pick $ choose (ironAmount + 1, 1000)
      applyToUniverse $ armWorker plId armAmount
      shouldHaveFailed,
  testProperty "Cannot arm with more than 8" $ armingProperty $ do
      plId <- findArmingPlayer
      armAmount <- pick $ choose (9, 1000)
      applyToUniverse $ armWorker plId armAmount
      shouldHaveFailed,
  testProperty "Cannot arm with zero or negative" $ armingProperty $ do
      plId <- findArmingPlayer
      armAmount <- pick $ choose (-1000, 0)
      applyToUniverse $ armWorker plId armAmount
      shouldHaveFailed,
  testProperty "Arming increases strength" $ armingProperty $ do
      plId <- findArmingPlayer
      originalUniverse <- getUniverse
      let workers = getWorkers originalUniverse plId
          originalStrengths = getWorkerStrength originalUniverse <$> workers
      ironAmount <- fmap getIronAmount $ getsUniverse getPlayerResources <*> pure plId
      pre $ ironAmount >= 1
      armAmount <- pick $ choose (1, min 8 ironAmount)
      applyToUniverse $ armWorker plId armAmount
      newUniverse <- getUniverse
      let newStrengths = getWorkerStrength newUniverse <$> workers
          increasedStrengths = filter (== armAmount) $ zipWith (-) newStrengths originalStrengths
      assert (length increasedStrengths == 1)
  ]

armingProperty :: UniversePropertyMonad a -> Property
armingProperty = propertyWithProperties $ defaultGeneratorProperties &
  withWorkplaceProbability WeaponMaking 20 &
  withArmingProbability 20 &
  withOtherWorkersNotDoneProbability 20

findArmingPlayer :: UniversePropertyMonad PlayerId
findArmingPlayer = do
  universe <- getUniverse
  plId <- preMaybe $ listToMaybe [plId | plId <- getPlayers universe, isArmingWorker universe plId]
  checkPlayerHasValidOccupants plId
  return plId
