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
    testProperty "Starting working start position selection" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      buildings <- getsUniverse currentlyBuiltBuildings <*> pure playerId
      assert $ buildings == [[Cave, Passage]],
    testProperty "Starting working and selecting invalid position fails" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      _ <- selectWrongPosition availableRockPositions playerId
      shouldHaveFailed,
    testProperty "Starting working and selecting valid position builds cave and passage" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      (pos, dir) <- selectCorrectPosition availableRockPositions playerId
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ Building Cave pos `elem` buildings
      assert $ Building Passage (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working collecting resources and canceling starts next player turn" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ collectResources playerId
      applyToUniverse $ finishAction playerId
      validateNextPlayer playerId,
    testProperty "Starting working and selecting position starts next player turn" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      checkPlayerHasValidOccupants playerId
      _ <- selectCorrectPosition availableRockPositions playerId
      validateNextPlayer playerId,
    testProperty "Starting working adds wood" $ universeProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, workplaceId) <- startWorkingInDigPassage
      let originalStone = getStoneAmount $ (getPlayerResources originalUniverse playerId)
          workplaceAmount = getStoneAmount $ getWorkplaceResources $ getWorkplaces originalUniverse ! workplaceId
      newStone <- getStoneAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      assert $ newStone == originalStone + workplaceAmount
  ]

startWorkingInDigPassage :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInDigPassage = startWorkingInWorkplaceType DigPassage
