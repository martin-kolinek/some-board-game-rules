module Workplaces.Farming where

import TestFramework
import TestHelpers
import Rules

import Prelude hiding (lookup)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

farmingWorkplaceTests :: TestTree
farmingWorkplaceTests = localOption (QuickCheckMaxRatio 200) $ testGroup "Farming workplace tests" $ [
    testProperty "Starting working starts cutting forest" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInFarming
      builtBuildings <- getsUniverse currentlyBuiltBuildings <*> pure playerId
      assert $ builtBuildings == [Grass, Field],
    testProperty "Canceling cutting forest is possible" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInFarming
      canCancel <- getsUniverse canCancelBuilding <*> pure playerId
      assert $ canCancel,
    testProperty "Planting crops is available after building" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInFarming
      applyToUniverse $ cancelSelection playerId
      plCrops <- getsUniverse isPlantingCrops <*> pure playerId
      assert $ plCrops,
    testProperty "Planting crops starts next player" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInFarming
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ cancelSelection playerId
      applyToUniverse $ plantCrops playerId []
      validateNextPlayer playerId
  ]

startWorkingInFarming :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInFarming = startWorkingInWorkplaceType Farming

