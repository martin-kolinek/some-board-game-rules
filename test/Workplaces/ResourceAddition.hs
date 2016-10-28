module Workplaces.ResourceAddition where

import Rules

import Test.Tasty
import Test.Tasty.QuickCheck
import TestFramework
import Test.QuickCheck.Monadic

resourceAdditionTests :: TestTree
resourceAdditionTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Resource addition tests" $ [
    testProperty "Starting working adds resources" $ universeProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, _) <- startWorkingInResourceAddition
      newUniverse <- getUniverse
      let originalResources = getPlayerResources originalUniverse playerId
          newResources = getPlayerResources newUniverse playerId
      assert $ getWoodAmount originalResources + 1 == getWoodAmount newResources
      assert $ getIronAmount originalResources + 1 == getIronAmount newResources
      assert $ getStoneAmount originalResources + 1 == getStoneAmount newResources
      assert $ getFoodAmount originalResources + 1 == getFoodAmount newResources
      assert $ getMoney originalResources + 2 == getMoney newResources,
    testProperty "Starting working stops turn" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInResourceAddition
      checkPlayerHasValidOccupants playerId
      validateNextPlayer playerId
  ]

startWorkingInResourceAddition :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInResourceAddition = startWorkingInWorkplaceType (== ResourceAddition)
