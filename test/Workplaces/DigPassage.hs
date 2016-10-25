module Workplaces.DigPassage where

import Rules

import Data.AdditiveGroup
import Data.Map ((!))

import Test.Tasty.QuickCheck
import Test.Tasty
import TestFramework
import Test.QuickCheck.Monadic
import RulesProperties

digPassageTests :: TestTree
digPassageTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Dig passage tests" [
    testProperty "Starting working start position selection" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      assert =<< getsUniverse isSelectingPosition <*> pure playerId,
    testProperty "Starting working and selecting invalid position fails" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      _ <- selectWrongPosition availableRockPositions playerId
      shouldHaveFailed,
    testProperty "Starting working and selecting valid position builds cave and passage" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      (pos, dir) <- selectCorrectPosition availableRockPositions playerId
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ Cave pos `elem` buildings
      assert $ Passage (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working and canceling starts next player turn" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigPassage
      checkPlayerHasValidOccupants playerId
      applyToUniverse cancelSelection
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
          DigPassage workplaceAmount = getWorkplaces originalUniverse ! workplaceId
      newStone <- getStoneAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      assert $ newStone == originalStone + workplaceAmount
  ]

isDigPassage :: WorkplaceData -> Bool
isDigPassage (DigPassage _) = True
isDigPassage _ = False

startWorkingInDigPassage :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInDigPassage = startWorkingInWorkplaceType isDigPassage
