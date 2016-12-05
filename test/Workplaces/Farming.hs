module Workplaces.Farming where

import TestFramework
import TestHelpers
import Rules

import Prelude hiding (lookup)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Data.List ((\\))
import Data.Map (keys, lookup)

farmingTests :: TestTree
farmingTests = localOption (QuickCheckMaxRatio 100) $ testGroup "Farming tests" $ [
    testProperty "Starting working makes planting crops available" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInFarming
      plCrops <- getsUniverse isPlantingCrops <*> pure playerId
      assert plCrops,
    testProperty "Planting crops adds crops" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInFarming
      cropsToPlant <- pickCropsToPlant playerId
      pre $ not $ null $ cropsToPlant
      applyToUniverse $ plantCrops cropsToPlant
      newCrops <- getsUniverse getPlantedCrops <*> pure playerId
      let verifyCrop (Potatoes, pos) = lookup pos newCrops == Just (PlantedCrop Potatoes 2)
          verifyCrop (Wheat, pos) = lookup pos newCrops == Just (PlantedCrop Wheat 3)
      assert $ all verifyCrop cropsToPlant,
    testProperty "Planting crops starts next player" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInFarming
      checkPlayerHasValidOccupants playerId
      cropsToPlant <- pickCropsToPlant playerId
      applyToUniverse $ plantCrops cropsToPlant
      validateNextPlayer playerId,
    testProperty "Planting crops not on fields is not possible" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInFarming
      let nonFieldPositions (Field _) = []
          nonFieldPositions (Forest pos) = [pos]
          nonFieldPositions (Grass pos) = [pos]
          nonFieldPositions (Rock pos) = [pos]
          nonFieldPositions (Cave pos) = [pos]
          nonFieldPositions (Passage pos) = [pos]
          nonFieldPositions (LivingRoom pos) = [pos]
          nonFieldPositions (InitialRoom pos) = [pos]
      buildingSpace <- getsUniverse getBuildingSpace <*> pure playerId
      validPositions <- pickPlantingPositions playerId
      invalidPositions <- pick $ shuffle $ nonFieldPositions =<< buildingSpace
      cropTypes <- pickCropTypes playerId True True
      pre $ length cropTypes > 1
      invalidCount <- pick $ choose (1, length cropTypes)
      validCount <- pick $ choose (0, length cropTypes - invalidCount)
      positions <- pick $ shuffle $ take invalidCount invalidPositions ++ take validCount validPositions
      applyToUniverse $ plantCrops (zip cropTypes positions)
      shouldHaveFailed,
    testProperty "Planting too many crops is not possible" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInFarming
      positions <- pickPlantingPositions playerId
      pre $ length positions >= 3
      tooManyType <- pick $ elements [Potatoes, Wheat]
      additionalCount <- pick $ choose (0, length positions - 3)
      additionalCrops <- pick $ vectorOf additionalCount $ elements [Potatoes, Wheat]
      crops <- pick $ shuffle $ additionalCrops ++ (replicate 3 tooManyType)
      applyToUniverse $ plantCrops (zip crops positions)
      shouldHaveFailed,
    testProperty "Planting more than had is not possible" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInFarming
      positions <- pickPlantingPositions playerId
      potatoCorrect <- pick $ elements [True, False]
      wheatCorrect <- pick $ elements [not potatoCorrect, False]
      crops <- pickCropTypes playerId wheatCorrect potatoCorrect
      pre $ length positions >= length crops
      applyToUniverse $ plantCrops (zip crops positions)
      shouldHaveFailed
  ]

startWorkingInFarming :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInFarming = startWorkingInWorkplaceType (== Farming)

pickPlantingPositions :: PlayerId -> UniversePropertyMonad [Position]
pickPlantingPositions plId = do
  buildingSpace <- getsUniverse getBuildingSpace <*> pure plId
  existingCrops <- getsUniverse getPlantedCrops <*> pure plId
  let fieldPosition (Field pos) = [pos]
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
