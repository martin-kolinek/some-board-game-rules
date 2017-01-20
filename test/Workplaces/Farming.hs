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
    testProperty "Starting working makes planting crops available" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInFarming
      plCrops <- getsUniverse isPlantingCrops <*> pure playerId
      assert plCrops,
    testProperty "Planting crops starts next player" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInFarming
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ plantCrops playerId []
      validateNextPlayer playerId
  ]

startWorkingInFarming :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInFarming = startWorkingInWorkplaceType (== Farming)

