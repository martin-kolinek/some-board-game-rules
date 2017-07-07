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
      buildingDescription <- pick $ elements buildingOptions
      let buildingExtractor = getBuildingExtractor buildingDescription
      checkResources buildingDescription
      (pos, dir) <- pickSpecificPosition buildingExtractor playerId
      let buildings = case buildingDescription of
            SingleSmallBuildingDesc b -> [SmallBuilding b pos]
            DoubleSmallBuildingDesc b1 b2 -> [SmallBuilding b1 pos, SmallBuilding b2 (pos ^+^ directionAddition dir)]
            LargeBuildingDesc b -> [LargeBuilding b pos dir]
      applyToUniverse $ buildBuildings playerId pos dir buildingDescription
      buildingSpace <- getsUniverse getBuildingSpace <*> pure playerId
      monitor $ counterexample $ "Checked buildings " ++ (show buildings)
      forM_ buildings $ \building ->
        assert $ building `elem` buildingSpace,
    testProperty "Building in correct place keeps only one building in one place" $ buildingProperty $ do
      (playerId, buildingOptions) <- findBuildingPlayer
      buildingDescription <- pick $ elements buildingOptions
      let buildingExtractor = getBuildingExtractor buildingDescription
      checkResources buildingDescription
      (pos, dir) <- pickSpecificPosition buildingExtractor playerId
      applyToUniverse $ buildBuildings playerId pos dir buildingDescription
      buildingSpace <- getsUniverse getBuildingSpace <*> pure playerId
      let isPositioned desiredPos (SmallBuilding _ realPos) = desiredPos == realPos
          isPositioned desiredPos (LargeBuilding _ realPos direction) = desiredPos == realPos || desiredPos == realPos ^+^ directionAddition direction
      forM_ availableBuildingPositions $ \position ->
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

findBuildingPlayer :: UniversePropertyMonad (PlayerId, [BuildingDescription])
findBuildingPlayer = do
  universe <- getUniverse
  (playerId, buildings) <- preMaybe $ listToMaybe [(plId, buildings) | plId <- getPlayers universe, let buildings = currentlyBuiltBuildings universe plId, not $ null buildings]
  checkPlayerHasValidOccupants playerId
  return (playerId, buildings)

getBuildingExtractor :: BuildingDescription -> Universe -> PlayerId -> [(Position, Direction)]
getBuildingExtractor (DoubleSmallBuildingDesc Grass Field) = availableForestPositions
getBuildingExtractor (DoubleSmallBuildingDesc Cave Passage) = availableRockPositions
getBuildingExtractor (DoubleSmallBuildingDesc Passage Cave) = availableRockPositions
getBuildingExtractor (DoubleSmallBuildingDesc Cave Cave) = availableRockPositions
getBuildingExtractor (SingleSmallBuildingDesc Cave) = availableSingleRockPositions
getBuildingExtractor (SingleSmallBuildingDesc Passage) = availableSingleRockPositions
getBuildingExtractor (SingleSmallBuildingDesc LivingRoom) = availableSingleCavePositions
getBuildingExtractor (SingleSmallBuildingDesc SmallPasture) = availableSingleGrassPositions
getBuildingExtractor (SingleSmallBuildingDesc Grass) = availableSingleForestPositions
getBuildingExtractor (SingleSmallBuildingDesc Field) = \universe plId ->
  let isNotOnBarn (pos, _) = not $ pos `elem` getBarns universe plId
  in filter isNotOnBarn $ availableSingleForestPositions universe plId
getBuildingExtractor (LargeBuildingDesc LargePasture) = availableGrassPositions
getBuildingExtractor bd = error $ "Unknown building type " ++ (show bd)

checkResources :: BuildingDescription -> UniversePropertyMonad ()
checkResources (SingleSmallBuildingDesc LivingRoom) =
  pre =<< getsUniverse currentPlayerHasEnoughResourcesForLivingRoom
checkResources _ = return ()
