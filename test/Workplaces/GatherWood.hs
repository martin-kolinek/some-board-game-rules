module Workplaces.GatherWood where

import Rules
import TestFramework
import TestHelpers

import Data.Map ((!))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

gatherWoodTests :: TestTree
gatherWoodTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Resource addition tests" $ [
    testProperty "Starting working adds resources" $ movingWorkerProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, workplaceId) <- startWorkingInGatherWood
      newUniverse <- getUniverse
      let originalResources = getPlayerResources originalUniverse playerId
          newResources = getPlayerResources newUniverse playerId
          woodAmount = getWoodAmount $ getWorkplaceResources $ (getWorkplaces originalUniverse ! workplaceId)
      assert $ getWoodAmount originalResources + woodAmount == getWoodAmount newResources,
    testProperty "Starting working stops turn" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInGatherWood
      checkPlayerHasValidOccupants playerId
      validateNextPlayer playerId
  ]

startWorkingInGatherWood :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInGatherWood = startWorkingInWorkplaceType GatherWood
