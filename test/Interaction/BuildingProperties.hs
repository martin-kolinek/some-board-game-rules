module Interaction.BuildingProperties where

import TestFramework
import Rules
import TestHelpers

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Data.Maybe
import Data.AdditiveGroup
import Control.Monad
import Data.Function

buildingTests :: TestTree
buildingTests = localOption (QuickCheckMaxRatio 200) $ testGroup "Building properties" $ [
    testProperty "Building in correct place places buildings" $ buildingProperty $ do
      (playerId, buildingOptions) <- findBuildingPlayer
      buildings <- pick $ elements buildingOptions
      let buildingExtractor = getBuildingExtractor buildings
      checkResources buildings
      (pos, dir) <- pickSpecificPosition buildingExtractor playerId
      applyToUniverse $ buildBuildings playerId pos dir buildings
      buildingSpace <- getsUniverse getBuildingSpace <*> pure playerId
      monitor $ counterexample $ "Checked buildings " ++ (show buildings)
      forM_ (zip buildings [pos, pos ^+^ directionAddition dir]) $ \(tp, position) ->
        assert $ (Building tp position) `elem` buildingSpace,
    testProperty "Building in correct place keeps only one building in one place" $ buildingProperty $ do
      (playerId, buildingOptions) <- findBuildingPlayer
      buildings <- pick $ elements buildingOptions
      let buildingExtractor = getBuildingExtractor buildings
      checkResources buildings
      (pos, dir) <- pickSpecificPosition buildingExtractor playerId
      applyToUniverse $ buildBuildings playerId pos dir buildings
      buildingSpace <- getsUniverse getBuildingSpace <*> pure playerId
      let isPositioned desiredPos (Building _ realPos) = desiredPos == realPos
      forM_ (zip buildings [pos, pos ^+^ directionAddition dir]) $ \(_, position) ->
        assert $ 1 == (length $ filter (isPositioned position) buildingSpace),
    testProperty "Building in incorrect place fails" $ buildingProperty $ do
      (playerId, buildingOptions) <- findBuildingPlayer
      buildings <- pick $ elements buildingOptions
      let buildingExtractor = getBuildingExtractor buildings
      checkResources buildings
      (pos, dir) <- pickWrongPosition buildingExtractor playerId
      applyToUniverse $ buildBuildings playerId pos dir buildings
      shouldHaveFailed
  ]

buildingProperty :: UniversePropertyMonad a -> Property
buildingProperty = propertyWithProperties $ defaultGeneratorProperties &
  withNoResourceChangeSteps

findBuildingPlayer :: UniversePropertyMonad (PlayerId, [[BuildingType]])
findBuildingPlayer = do
  universe <- getUniverse
  (playerId, buildings) <- preMaybe $ listToMaybe [(plId, buildings) | plId <- getPlayers universe, let buildings = currentlyBuiltBuildings universe plId, not $ null buildings]
  checkPlayerHasValidOccupants playerId
  return (playerId, buildings)

getBuildingExtractor :: [BuildingType] -> Universe -> PlayerId -> [(Position, Direction)]
getBuildingExtractor buildings = singleBuildingExtractor $ head buildings
  where singleBuildingExtractor building
          | building `elem` [Grass, Field] = if length buildings == 1 then availableSingleForestPositions else availableForestPositions
          | building `elem` [Passage, Cave] = if length buildings == 1 then availableSingleRockPositions else availableRockPositions
          | building `elem` [LivingRoom] = availableSingleCavePositions
          | building == SmallPasture = availableSingleGrassPositions
          | otherwise = const $ const []

checkResources :: [BuildingType] -> UniversePropertyMonad ()
checkResources [LivingRoom] =
  pre =<< getsUniverse currentPlayerHasEnoughResourcesForLivingRoom
checkResources _ = return ()
