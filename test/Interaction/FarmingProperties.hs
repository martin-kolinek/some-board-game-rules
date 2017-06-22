module Interaction.FarmingProperties where

import TestFramework
import TestHelpers
import Rules

import Prelude hiding (lookup)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Data.List ((\\))
import Data.Map (keys, lookup)
import Data.Maybe (listToMaybe)
import Data.Function ((&))
import Data.AdditiveGroup

farmingTests :: TestTree
farmingTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Farming properties" $ [
    testProperty "Planting crops adds crops" $ farmingProperty $ do
      playerId <- findFarmingPlayer
      cropsToPlant <- pickCropsToPlant playerId
      pre $ not $ null $ cropsToPlant
      applyToUniverse $ plantCrops playerId cropsToPlant
      newCrops <- getsUniverse getPlantedCrops <*> pure playerId
      let verifyCrop (Potatoes, pos) = lookup pos newCrops == Just (PlantedCrop Potatoes 2)
          verifyCrop (Wheat, pos) = lookup pos newCrops == Just (PlantedCrop Wheat 3)
      assert $ all verifyCrop cropsToPlant,
    testProperty "Planting crops not on fields is not possible" $ farmingProperty $ do
      playerId <- findFarmingPlayer
      let nonFieldPositions (SmallBuilding buildingType pos) = if buildingType == Field then [] else [pos]
          nonFieldPositions (LargeBuilding _ pos dir) = [pos, directionAddition dir ^+^ pos]
      buildingSpace <- getsUniverse getBuildingSpace <*> pure playerId
      validPositions <- pickPlantingPositions playerId
      invalidPositions <- pick $ shuffle $ nonFieldPositions =<< buildingSpace
      cropTypes <- pickCropTypes playerId True True
      pre $ length cropTypes > 1
      invalidCount <- pick $ choose (1, length cropTypes)
      validCount <- pick $ choose (0, length cropTypes - invalidCount)
      positions <- pick $ shuffle $ take invalidCount invalidPositions ++ take validCount validPositions
      applyToUniverse $ plantCrops playerId (zip cropTypes positions)
      shouldHaveFailed,
    testProperty "Planting too many crops is not possible" $ farmingProperty $ do
      playerId <- findFarmingPlayer
      positions <- pickPlantingPositions playerId
      pre $ length positions >= 3
      tooManyType <- pick $ elements [Potatoes, Wheat]
      additionalCount <- pick $ choose (0, length positions - 3)
      additionalCrops <- pick $ vectorOf additionalCount $ elements [Potatoes, Wheat]
      crops <- pick $ shuffle $ additionalCrops ++ (replicate 3 tooManyType)
      applyToUniverse $ plantCrops playerId (zip crops positions)
      shouldHaveFailed,
    testProperty "Planting more than had is not possible" $ farmingProperty $ do
      playerId <- findFarmingPlayer
      positions <- pickPlantingPositions playerId
      potatoCorrect <- pick $ elements [True, False]
      wheatCorrect <- pick $ elements [not potatoCorrect, False]
      crops <- pickCropTypes playerId wheatCorrect potatoCorrect
      pre $ length positions >= length crops
      applyToUniverse $ plantCrops playerId (zip crops positions)
      shouldHaveFailed,
    testProperty "Planting crops subtracts resources" $ farmingProperty $ do
      playerId <- findFarmingPlayer
      originalResources <- getsUniverse getPlayerResources <*> pure playerId
      crops <- pickCropsToPlant playerId
      pre $ not $ null $ crops
      applyToUniverse $ plantCrops playerId crops
      newResources <- getsUniverse getPlayerResources <*> pure playerId
      let potatoCount = length $ filter (== Potatoes) $ fst <$> crops
          wheatCount = length $ filter (== Wheat) $ fst <$> crops
      assert $ potatoCount == getPotatoAmount originalResources - getPotatoAmount newResources
      assert $ wheatCount == getWheatAmount originalResources - getWheatAmount newResources
  ]

farmingProperty :: UniversePropertyMonad a -> Property
farmingProperty = propertyWithProperties $ defaultGeneratorProperties &
  withWorkplaceProbability Farming 20 &
  withFarmingProbability 20 &
  withNoResourceChangeSteps

findFarmingPlayer :: UniversePropertyMonad PlayerId
findFarmingPlayer = do
  universe <- getUniverse
  plId <- preMaybe $ listToMaybe [plId | plId <- getPlayers universe, isPlantingCrops universe plId]
  checkPlayerHasValidOccupants plId
  return plId

pickPlantingPositions :: PlayerId -> UniversePropertyMonad [Position]
pickPlantingPositions plId = do
  buildingSpace <- getsUniverse getBuildingSpace <*> pure plId
  existingCrops <- getsUniverse getPlantedCrops <*> pure plId
  let fieldPosition (SmallBuilding Field pos) = [pos]
      fieldPosition _ = []
      allFieldPositions = buildingSpace >>= fieldPosition
      freeFieldPositions = allFieldPositions \\ keys existingCrops
  pick $ shuffle freeFieldPositions

pickCropsToPlant :: PlayerId -> UniversePropertyMonad [(CropType, Position)]
pickCropsToPlant plId = do
  positions <- pickPlantingPositions plId
  count <- pick $ choose (0, 4)
  cropTypes <- pickCropTypes plId True True
  return $ take count $ zip cropTypes positions

pickCropTypes :: PlayerId -> Bool -> Bool -> UniversePropertyMonad [CropType]
pickCropTypes plId wheatCorrect potatoCorrect = do
  resources <- getsUniverse getPlayerResources <*> pure plId
  let getAmount correct getter = if correct then pick $ choose (0, min 2 (getter resources))
        else do
          pre $ getter resources < 2
          pick $ choose (getter resources + 1, 2)
  potatoCount <- getAmount potatoCorrect getPotatoAmount
  wheatCount <- getAmount wheatCorrect getWheatAmount
  pick $ shuffle (replicate potatoCount Potatoes ++ replicate wheatCount Wheat)
