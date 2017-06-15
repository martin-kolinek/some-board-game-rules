module Workplaces.CutForest where

import Rules
import TestFramework
import TestHelpers

import Data.AdditiveGroup
import Data.Map ((!))
import Test.Tasty.QuickCheck
import Test.Tasty
import Test.QuickCheck.Monadic

cutForestTests :: TestTree
cutForestTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Cut forest tests" $ [
    testProperty "Starting working start position selection" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInCutForest
      buildings <- getsUniverse currentlyBuiltBuildings <*> pure playerId
      assert $ buildings == [[Grass, Field]],
    testProperty "Starting working and selecting invalid position fails" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInCutForest
      _ <- selectWrongPosition availableForestPositions playerId
      shouldHaveFailed,
    testProperty "Starting working and selecting valid position builds field and grass" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInCutForest
      checkPlayerHasValidOccupants playerId
      (pos, dir) <- selectCorrectPosition availableForestPositions playerId
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ Building Grass pos `elem` buildings
      assert $ Building Field (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working collecting resources and canceling starts next player turn" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInCutForest
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ collectResources playerId
      applyToUniverse $ finishAction playerId
      validateNextPlayer playerId,
    testProperty "Starting working collecting resources and selecting position starts next player turn" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInCutForest
      checkPlayerHasValidOccupants playerId
      _ <- selectCorrectPosition availableForestPositions playerId
      applyToUniverse $ collectResources playerId
      validateNextPlayer playerId,
    testProperty "Starting working and collecting resources adds wood" $ movingWorkerProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, workplaceId) <- startWorkingInCutForest
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ collectResources playerId
      let originalWood = getWoodAmount $ (getPlayerResources originalUniverse playerId)
          workplaceAmount = getWoodAmount $ getWorkplaceResources (getWorkplaces originalUniverse ! workplaceId)
      newWood <- getWoodAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      assert $ newWood == originalWood + workplaceAmount
  ]

startWorkingInCutForest :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInCutForest = startWorkingInWorkplaceType CutForest
