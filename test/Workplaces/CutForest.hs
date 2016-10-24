module Workplaces.CutForest where

import Rules

import Data.AdditiveGroup
import Data.Map ((!))

import Test.Tasty.QuickCheck
import Test.Tasty
import Test.QuickCheck.Monadic
import TestFramework
import RulesProperties
import Generators

cutForestTests :: TestTree
cutForestTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Cut forest tests" $ [
    testProperty "Starting working start position selection" $ cutForestProperty $ \playerId workerId workplaceId -> do
      applyToUniverse $ startWorking workerId workplaceId
      assert =<< getsUniverse isSelectingPosition <*> pure playerId,
    testProperty "Starting working and selecting invalid position fails" $ cutForestProperty $ \playerId workerId workplaceId -> do
      applyToUniverse $ startWorking workerId workplaceId
      (pos, dir) <- pickWrongPosition availableForestPositions playerId
      applyToUniverse $ selectPosition pos dir
      shouldHaveFailed,
    testProperty "Starting working and selecting valid position builds field and grass" $ cutForestProperty $ \playerId workerId workplaceId -> do
      applyToUniverse $ startWorking workerId workplaceId
      (pos, dir) <- pickSpecificPosition availableForestPositions playerId
      applyToUniverse $ selectPosition pos dir
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ Grass pos `elem` buildings
      assert $ Field (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working and canceling starts next player turn" $ cutForestProperty $ \playerId workerId workplaceId -> do
      prePlayerHasValidOccupants playerId
      applyToUniverse $ startWorking workerId workplaceId
      applyToUniverse cancelSelection
      validateNextPlayer playerId,
    testProperty "Starting working and selecting position starts next player turn" $ cutForestProperty $ \playerId workerId workplaceId -> do
      prePlayerHasValidOccupants playerId
      applyToUniverse $ startWorking workerId workplaceId
      (pos, dir) <- pickSpecificPosition availableForestPositions playerId
      applyToUniverse $ selectPosition pos dir
      validateNextPlayer playerId,
    testProperty "Starting working adds wood" $ cutForestProperty $ \playerId workerId workplaceId -> do
      originalWood <- getWoodAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      (CutForest workplaceAmount) <- (! workplaceId) <$> getsUniverse getWorkplaces
      applyToUniverse $ startWorking workerId workplaceId
      newWood <- getWoodAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      assert $ newWood == originalWood + workplaceAmount
  ]

cutForestProperty :: (PlayerId -> WorkerId -> WorkplaceId -> UniversePropertyMonad a) -> ArbitraryUniverse -> Property
cutForestProperty action = universeProperty $ do
  workplaceId <- pickAnyEmptyWorkplace findEmptyCutForestWorkplaces
  (playerId, workerId) <- pickWorkerToMove
  action playerId workerId workplaceId

