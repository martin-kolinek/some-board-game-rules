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
import qualified Data.Set as S

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
      shouldHaveFailed,
    testProperty "Building barn is possible" $ barnBuildingProperty $ do
      playerId <- findBarnBuildingPlayer
      pos <- pickValidBarnPosition playerId
      checkCanBuildBarn playerId
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ buildBarn playerId pos
      barns <- getsUniverse getBarns <*> pure playerId
      assert $ pos `elem` barns,
    testProperty "Building barn in invalid position is not possible" $ barnBuildingProperty $ do
      playerId <- findBarnBuildingPlayer
      pos <- pickInvalidBarnPosition playerId
      checkPlayerHasValidOccupants playerId
      checkCanBuildBarn playerId
      applyToUniverse $ buildBarn playerId pos
      shouldHaveFailed,
    testProperty "Building barn when there are already two barns is not possible" $ barnBuildingProperty $ do
      playerId <- findBarnBuildingPlayer
      barns <- getsUniverse getBarns <*> pure playerId
      assert $ length barns < 2
  ]

buildingProperty :: UniversePropertyMonad a -> Property
buildingProperty = propertyWithProperties $ defaultGeneratorProperties &
  withNoResourceChangeSteps

barnBuildingProperty :: UniversePropertyMonad a -> Property
barnBuildingProperty = propertyWithProperties $ defaultGeneratorProperties &
  withNoResourceChangeSteps

findBuildingPlayer :: UniversePropertyMonad (PlayerId, [BuildingDescription])
findBuildingPlayer = do
  universe <- getUniverse
  (playerId, buildings) <- preMaybe $ listToMaybe [(plId, buildings) | plId <- getPlayers universe, let buildings = currentlyBuiltBuildings universe plId, not $ null buildings]
  checkPlayerHasValidOccupants playerId
  return (playerId, buildings)

findBarnBuildingPlayer :: UniversePropertyMonad PlayerId
findBarnBuildingPlayer = do
  universe <- getUniverse
  preMaybe $ listToMaybe [plId | plId <- getPlayers universe, canBuildBarn universe plId]

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

pickValidBarnPosition :: PlayerId -> UniversePropertyMonad Position
pickValidBarnPosition plId = do
  positions <- getsUniverse findValidBarnPositions <*> pure plId
  pre $ not $ null $ positions
  pick $ elements positions

pickInvalidBarnPosition :: PlayerId -> UniversePropertyMonad Position
pickInvalidBarnPosition plId = do
  positions <- getsUniverse findValidBarnPositions <*> pure plId
  pick $ elements $ S.toList $ (S.fromList availableBuildingPositions) S.\\ (S.fromList positions)

