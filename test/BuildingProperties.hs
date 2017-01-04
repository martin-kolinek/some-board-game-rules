module BuildingProperties where

import TestFramework
import Rules
import TestHelpers

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Data.Maybe
import Data.AdditiveGroup
import Control.Monad

buildingTests :: TestTree
buildingTests = testGroup "Building properties" $ [
    testProperty "Building in correct place places buildings" $ universeProperty $ do
      (playerId, buildings) <- findBuildingPlayer
      let buildingExtractor = getBuildingExtractor buildings
      (pos, dir) <- selectCorrectPosition buildingExtractor playerId
      buildingSpace <- getsUniverse getBuildingSpace <*> pure playerId
      forM_ (zip buildings [pos, pos ^+^ directionAddition dir]) $ \(tp, position) ->
        assert $ (Building tp position) `elem` buildingSpace,
    testProperty "Building in correct place keeps only one building in one place" $ universeProperty $ do
      (playerId, buildings) <- findBuildingPlayer
      let buildingExtractor = getBuildingExtractor buildings
      (pos, dir) <- selectCorrectPosition buildingExtractor playerId
      buildingSpace <- getsUniverse getBuildingSpace <*> pure playerId
      let isPositioned desiredPos (Building _ realPos) = desiredPos == realPos
      forM_ (zip buildings [pos, pos ^+^ directionAddition dir]) $ \(_, position) ->
        assert $ 1 == (length $ filter (isPositioned position) buildingSpace),
    testProperty "Building in incorrect place fails" $ universeProperty $ do
      (playerId, buildings) <- findBuildingPlayer
      let buildingExtractor = getBuildingExtractor buildings
      _ <- selectWrongPosition buildingExtractor playerId
      shouldHaveFailed
  ]

findBuildingPlayer :: UniversePropertyMonad (PlayerId, [BuildingType])
findBuildingPlayer = do
  universe <- getUniverse
  preMaybe $ listToMaybe [(plId, buildings) | plId <- getPlayers universe, let buildings = currentlyBuiltBuildings universe plId, not $ null buildings]

getBuildingExtractor :: [BuildingType] -> Universe -> PlayerId -> [(Position, Direction)]
getBuildingExtractor buildings = singleBuildingExtractor $ head buildings
  where singleBuildingExtractor building
          | building `elem` [Grass, Field] = availableForestPositions
          | building `elem` [Passage, Cave] = availableRockPositions
          | building `elem` [LivingRoom] = availableSingleCavePositions
          | otherwise = const $ const []
