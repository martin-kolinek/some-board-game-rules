module Workplaces.DigPassage where

import Rules
import TestFramework
import TestHelpers

import Data.AdditiveGroup
import Data.Map ((!))

import Test.Tasty.QuickCheck
import Test.Tasty
import Test.QuickCheck.Monadic

digPassageTests :: TestTree
digPassageTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Dig passage tests" [
    testProperty "Starting working start position selection" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      buildings <- getsUniverse currentlyBuiltBuildings <*> pure playerId
      assert $ buildings == [DoubleSmallBuildingDesc Cave Passage],
    testProperty "Starting working and selecting invalid position fails" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      _ <- selectWrongPosition availableRockPositions playerId
      shouldHaveFailed,
    testProperty "Starting working and selecting valid position builds cave and passage" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      checkPlayerHasValidOccupants playerId
      (pos, dir) <- selectCorrectPosition availableRockPositions playerId
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ SmallBuilding Cave pos `elem` buildings
      assert $ SmallBuilding Passage (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working collecting resources and canceling starts next player turn" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ collectResources playerId
      applyToUniverse $ finishAction playerId
      validateNextPlayer playerId,
    testProperty "Starting working collecting resources and selecting position starts next player turn" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ collectResources playerId
      _ <- selectCorrectPosition availableRockPositions playerId
      validateNextPlayer playerId,
    testProperty "Starting working adds stone" $ movingWorkerProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, workplaceId) <- startWorkingInDigPassage
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ collectResources playerId
      let originalStone = getStoneAmount $ (getPlayerResources originalUniverse playerId)
          workplaceAmount = getStoneAmount $ getWorkplaceResources $ getWorkplaces originalUniverse ! workplaceId
      newStone <- getStoneAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      assert $ newStone == originalStone + workplaceAmount
  ]

startWorkingInDigPassage :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInDigPassage = startWorkingInWorkplaceType DigPassage
