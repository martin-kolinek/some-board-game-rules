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
    testProperty "Starting working start position selection" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInCutForest
      assert =<< getsUniverse isSelectingPosition <*> pure playerId,
    testProperty "Starting working and selecting invalid position fails" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInCutForest
      _ <- selectWrongPosition availableForestPositions playerId
      shouldHaveFailed,
    testProperty "Starting working and selecting valid position builds field and grass" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInCutForest
      (pos, dir) <- selectCorrectPosition availableForestPositions playerId
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ Grass pos `elem` buildings
      assert $ Field (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working and canceling starts next player turn" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInCutForest
      checkPlayerHasValidOccupants playerId
      applyToUniverse cancelSelection
      validateNextPlayer playerId,
    testProperty "Starting working and selecting position starts next player turn" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInCutForest
      checkPlayerHasValidOccupants playerId
      _ <- selectCorrectPosition availableForestPositions playerId
      validateNextPlayer playerId,
    testProperty "Starting working adds wood" $ universeProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, workplaceId) <- startWorkingInCutForest
      let originalWood = getWoodAmount $ (getPlayerResources originalUniverse playerId)
          CutForest workplaceAmount = (getWorkplaces originalUniverse ! workplaceId)
      newWood <- getWoodAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      assert $ newWood == originalWood + workplaceAmount
  ]

isCutForest :: WorkplaceData -> Bool
isCutForest (CutForest _) = True
isCutForest _ = False

startWorkingInCutForest :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInCutForest = startWorkingInWorkplaceType isCutForest
