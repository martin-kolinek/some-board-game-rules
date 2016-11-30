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
    testProperty "Starting working adds resources" $ universeProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, workplaceId) <- startWorkingInGatherWood
      newUniverse <- getUniverse
      let originalResources = getPlayerResources originalUniverse playerId
          newResources = getPlayerResources newUniverse playerId
          (GatherWood woodAmount) = (getWorkplaces originalUniverse ! workplaceId)
      assert $ getWoodAmount originalResources + woodAmount == getWoodAmount newResources,
    testProperty "Starting working stops turn" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInGatherWood
      checkPlayerHasValidOccupants playerId
      validateNextPlayer playerId
  ]

isGatherWood :: WorkplaceData -> Bool
isGatherWood (GatherWood _) = True
isGatherWood _ = False

startWorkingInGatherWood :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInGatherWood = startWorkingInWorkplaceType isGatherWood
