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
    testProperty "Starting working makes correct buildings available" $ universeProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        buildings <- getsUniverse currentlyBuiltBuildings <*> pure playerId
        assert $ (S.fromList buildings) == (S.fromList [[Cave, Passage], [Cave, Cave]]),
    testProperty "Starting working collecting resources and finishing" $ universeProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        checkPlayerHasValidOccupants playerId
        applyToUniverse $ collectResources playerId
        applyToUniverse $ finishAction playerId
        validateNextPlayer playerId,
    testProperty "Starting working, and digging cave adds cave" $ universeProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        checkPlayerHasValidOccupants playerId
        (pos, dir) <- pickSpecificPosition availableRockPositions playerId
        applyToUniverse $ buildBuildings playerId pos dir [Cave, Cave]
        buildings <- getsUniverse getBuildingSpace <*> pure playerId
        assert $ Building Cave pos `elem` buildings
        assert $ Building Cave (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working, and digging passage adds cave" $ universeProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        checkPlayerHasValidOccupants playerId
        (pos, dir) <- pickSpecificPosition availableRockPositions playerId
        applyToUniverse $ buildBuildings playerId pos dir [Cave, Passage]
        buildings <- getsUniverse getBuildingSpace <*> pure playerId
        assert $ Building Cave pos `elem` buildings
        assert $ Building Passage (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working and collecting resources adds stone" $ universeProperty $ do
        originalUniverse <- getUniverse
        (playerId, _, workplaceId) <- startWorkingInDigCave
        checkPlayerHasValidOccupants playerId
        applyToUniverse $ collectResources playerId
        let originalStone = getStoneAmount $ (getPlayerResources originalUniverse playerId)
            workplaceAmount = getStoneAmount $ getWorkplaceResources $ getWorkplaces originalUniverse ! workplaceId
        newStone <- getStoneAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
        assert $ newStone == originalStone + workplaceAmount,
    testProperty "Starting working, and selecting invalid position fails" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigCave
      _ <- selectWrongPosition availableRockPositions playerId
      shouldHaveFailed,
    testProperty "Starting working, and selecting correct position starts next player" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigCave
      checkPlayerHasValidOccupants playerId
      _ <- selectCorrectPosition availableRockPositions playerId
      applyToUniverse $ finishAction playerId
      validateNextPlayer playerId
  ]

startWorkingInDigCave :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInDigCave = startWorkingInWorkplaceType DigCave
