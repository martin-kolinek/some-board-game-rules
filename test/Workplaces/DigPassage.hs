module Workplaces.DigPassage where

import Rules

import Data.AdditiveGroup
import Data.Map ((!))

import Test.Tasty.QuickCheck
import Test.Tasty
import TestFramework
import Test.QuickCheck.Monadic
import Generators
import RulesProperties

digPassageTests :: TestTree
digPassageTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Dig passage tests" [
    testProperty "Starting working start position selection" $ digPassageProperty $ \playerId workerId workplaceId -> do
      applyToUniverse $ startWorking workerId workplaceId
      assert =<< getsUniverse isSelectingPosition <*> pure playerId,
    testProperty "Starting working and selecting invalid position fails" $ digPassageProperty $ \playerId workerId workplaceId -> do
      applyToUniverse $ startWorking workerId workplaceId
      (pos, dir) <- pickWrongPosition availableRockPositions playerId
      applyToUniverse $ selectPosition pos dir
      shouldHaveFailed,
    testProperty "Starting working and selecting valid position builds cave and passage" $ digPassageProperty $ \playerId workerId workplaceId -> do
      applyToUniverse $ startWorking workerId workplaceId
      (pos, dir) <- pickSpecificPosition availableRockPositions playerId
      applyToUniverse $ selectPosition pos dir
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ Cave pos `elem` buildings
      assert $ Passage (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working and canceling starts next player turn" $ digPassageProperty $ \playerId workerId workplaceId -> do
      prePlayerHasValidOccupants playerId
      applyToUniverse $ startWorking workerId workplaceId
      applyToUniverse cancelSelection
      validateNextPlayer playerId,
    testProperty "Starting working and selecting position starts next player turn" $ digPassageProperty $ \playerId workerId workplaceId -> do
      prePlayerHasValidOccupants playerId
      applyToUniverse $ startWorking workerId workplaceId
      (pos, dir) <- pickSpecificPosition availableRockPositions playerId
      applyToUniverse $ selectPosition pos dir
      validateNextPlayer playerId,
    testProperty "Starting working adds wood" $ digPassageProperty $ \playerId workerId workplaceId -> do
      originalStone <- getStoneAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      (DigPassage workplaceAmount) <- (! workplaceId) <$> getsUniverse getWorkplaces
      applyToUniverse $ startWorking workerId workplaceId
      newStone <- getStoneAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      assert $ newStone == originalStone + workplaceAmount
  ]

digPassageProperty :: (PlayerId -> WorkerId -> WorkplaceId -> UniversePropertyMonad a) -> ArbitraryUniverse -> Property
digPassageProperty action = universeProperty $ do
  workplaceId <- pickAnyEmptyWorkplace findEmptyDigPassageWorkplaces
  (playerId, workerId) <- pickWorkerToMove
  action playerId workerId workplaceId
