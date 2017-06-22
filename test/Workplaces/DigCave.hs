module Workplaces.DigCave where

import Rules
import TestFramework
import TestHelpers

import Data.AdditiveGroup
import Data.Map ((!))

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.Set as S

digCaveTests :: TestTree
digCaveTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Dig cave tests" $ [
    testProperty "Starting working makes correct buildings available" $ movingWorkerProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        buildings <- getsUniverse currentlyBuiltBuildings <*> pure playerId
        assert $ (S.fromList buildings) == (S.fromList [(DoubleSmallBuildingDesc Cave Passage), (DoubleSmallBuildingDesc Cave Cave)]),
    testProperty "Starting working collecting resources and finishing" $ movingWorkerProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        checkPlayerHasValidOccupants playerId
        applyToUniverse $ collectResources playerId
        applyToUniverse $ finishAction playerId
        validateNextPlayer playerId,
    testProperty "Starting working, and digging cave adds cave" $ movingWorkerProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        checkPlayerHasValidOccupants playerId
        (pos, dir) <- pickSpecificPosition availableRockPositions playerId
        applyToUniverse $ buildBuildings playerId pos dir (DoubleSmallBuildingDesc Cave Cave)
        buildings <- getsUniverse getBuildingSpace <*> pure playerId
        assert $ SmallBuilding Cave pos `elem` buildings
        assert $ SmallBuilding Cave (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working, and digging passage adds cave" $ movingWorkerProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        checkPlayerHasValidOccupants playerId
        (pos, dir) <- pickSpecificPosition availableRockPositions playerId
        applyToUniverse $ buildBuildings playerId pos dir (DoubleSmallBuildingDesc Cave Passage)
        buildings <- getsUniverse getBuildingSpace <*> pure playerId
        assert $ SmallBuilding Cave pos `elem` buildings
        assert $ SmallBuilding Passage (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working and collecting resources adds stone" $ movingWorkerProperty $ do
        originalUniverse <- getUniverse
        (playerId, _, workplaceId) <- startWorkingInDigCave
        checkPlayerHasValidOccupants playerId
        applyToUniverse $ collectResources playerId
        let originalStone = getStoneAmount $ (getPlayerResources originalUniverse playerId)
            workplaceAmount = getStoneAmount $ getWorkplaceResources $ getWorkplaces originalUniverse ! workplaceId
        newStone <- getStoneAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
        assert $ newStone == originalStone + workplaceAmount,
    testProperty "Starting working, and selecting invalid position fails" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInDigCave
      _ <- selectWrongPosition availableRockPositions playerId
      shouldHaveFailed,
    testProperty "Starting working, and selecting correct position starts next player" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInDigCave
      checkPlayerHasValidOccupants playerId
      _ <- selectCorrectPosition availableRockPositions playerId
      applyToUniverse $ finishAction playerId
      validateNextPlayer playerId
  ]

startWorkingInDigCave :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInDigCave = startWorkingInWorkplaceType DigCave
