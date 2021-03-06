module Workplaces.GatherFood where

import Rules
import TestFramework
import TestHelpers

import Data.AdditiveGroup
import Data.Map ((!))
import Test.Tasty.QuickCheck
import Test.Tasty
import Test.QuickCheck.Monadic

gatherFoodTests :: TestTree
gatherFoodTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Gather food tests" $ [
    testProperty "Starting working start position selection" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInGatherFood
      buildings <- getsUniverse currentlyBuiltBuildings <*> pure playerId
      assert $ buildings == [DoubleSmallBuildingDesc Grass Field],
    testProperty "Starting working and selecting invalid position fails" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInGatherFood
      _ <- selectWrongPosition availableForestPositions playerId
      shouldHaveFailed,
    testProperty "Starting working and selecting valid position builds field and grass" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInGatherFood
      checkPlayerHasValidOccupants playerId
      (pos, dir) <- selectCorrectPosition availableForestPositions playerId
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ SmallBuilding Grass pos `elem` buildings
      assert $ SmallBuilding Field (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working collecting resources and canceling starts next player turn" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInGatherFood
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ collectResources playerId
      applyToUniverse $ finishAction playerId
      validateNextPlayer playerId,
    testProperty "Starting working and selecting position starts next player turn" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInGatherFood
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ collectResources playerId
      _ <- selectCorrectPosition availableForestPositions playerId
      validateNextPlayer playerId,
    testProperty "Starting working adds wheat and food" $ movingWorkerProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, workplaceId) <- startWorkingInGatherFood
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ collectResources playerId
      let originalFood = getFoodAmount $ (getPlayerResources originalUniverse playerId)
          workplaceAmount = getFoodAmount $ getWorkplaceResources $ (getWorkplaces originalUniverse ! workplaceId)
          originalWheat = getWheatAmount $ (getPlayerResources originalUniverse playerId)
          originalStone = getStoneAmount $ (getPlayerResources originalUniverse playerId)
      newFood <- getFoodAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      newWheat <- getWheatAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      newStone <- getStoneAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      assert $ newFood == originalFood + workplaceAmount
      assert $ newWheat == originalWheat + 1
      assert $ newStone == originalStone
  ]

startWorkingInGatherFood :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInGatherFood = startWorkingInWorkplaceType GatherFood
