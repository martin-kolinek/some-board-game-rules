module Workplaces.GatherFood where

import Rules

import Data.AdditiveGroup
import Data.Map ((!))

import Test.Tasty.QuickCheck
import Test.Tasty
import Test.QuickCheck.Monadic
import TestFramework
import RulesProperties

gatherFoodTests :: TestTree
gatherFoodTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Gather food tests" $ [
    testProperty "Starting working start position selection" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInGatherFood
      assert =<< getsUniverse isSelectingPosition <*> pure playerId,
    testProperty "Starting working and selecting invalid position fails" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInGatherFood
      _ <- selectWrongPosition availableForestPositions playerId
      shouldHaveFailed,
    testProperty "Starting working and selecting valid position builds field and grass" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInGatherFood
      (pos, dir) <- selectCorrectPosition availableForestPositions playerId
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ Grass pos `elem` buildings
      assert $ Field (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working and canceling starts next player turn" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInGatherFood
      checkPlayerHasValidOccupants playerId
      applyToUniverse cancelSelection
      validateNextPlayer playerId,
    testProperty "Starting working and selecting position starts next player turn" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInGatherFood
      checkPlayerHasValidOccupants playerId
      _ <- selectCorrectPosition availableForestPositions playerId
      validateNextPlayer playerId,
    testProperty "Starting working adds wood" $ universeProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, workplaceId) <- startWorkingInGatherFood
      let originalFood = getFoodAmount $ (getPlayerResources originalUniverse playerId)
          GatherFood workplaceAmount = (getWorkplaces originalUniverse ! workplaceId)
          originalWheat = getWheatAmount $ (getPlayerResources originalUniverse playerId)
      newFood <- getFoodAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      newWheat <- getWheatAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
      assert $ newFood == originalFood + workplaceAmount
      assert $ newWheat == originalWheat + 1
  ]

isGatherFood :: WorkplaceData -> Bool
isGatherFood (GatherFood _) = True
isGatherFood _ = False

startWorkingInGatherFood :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInGatherFood = startWorkingInWorkplaceType isGatherFood
