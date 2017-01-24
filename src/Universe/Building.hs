module Universe.Building where

import Control.Lens hiding (universe)
import Data.Map
import Data.Maybe
import Data.AdditiveGroup

import Universe.Player
import Universe
import Player
import Building
import Resources

getOccupantErrors :: Universe -> PlayerId -> [OccupantError]
getOccupantErrors universe player = toListOf (players . ix player . folding verifyOccupants) universe

getBuildingSpace :: Universe -> PlayerId -> [Building]
getBuildingSpace universe player = toListOf (players . ix player . buildingSpace . folding getBuildings) universe

getBuildingOccupants :: Universe -> PlayerId -> BuildingOccupants
getBuildingOccupants universe player = fromMaybe empty $ universe ^? (players . ix player . buildingSpace . buildingSpaceOccupants)

getAllOccupants :: Universe -> PlayerId -> [BuildingOccupant]
getAllOccupants universe player = toListOf (players . ix player . folding getPlayerPossibleOccupants) universe

getPlantedCrops :: Universe -> PlayerId -> Map Position PlantedCrop
getPlantedCrops universe player = fromMaybe empty $ universe ^? (players . ix player . buildingSpace . buildingSpaceCrops)

getPlayerPossibleOccupants :: PlayerData -> [BuildingOccupant]
getPlayerPossibleOccupants playerData =
  (DogOccupant <$> toListOf (playerAnimals . dogs . traverse) playerData) ++ (WorkerOccupant <$> toListOf (workers . folding keys) playerData)

currentPlayerCanBuildRoom :: Universe -> Bool
currentPlayerCanBuildRoom universe = has (currentPlayerData . buildingSpace . to getBuildings . traverse . filtered ((==Cave) . getBuildingType)) universe &&
  has (currentPlayerData . playerResources . woodAmount . filtered (>=4)) universe &&
  has (currentPlayerData . playerResources . stoneAmount . filtered (>=3)) universe

playerCanHireWorker :: PlayerData -> Bool
playerCanHireWorker playerData = canSupportAdditionalWorker (getPlayerPossibleOccupants playerData) (playerData ^. buildingSpace)

playerCanBuildBuilding :: PlayerData -> BuildingType -> Bool
playerCanBuildBuilding playerData buildingType = hasUnderlyingBuilding && hasEnoughResources
  where hasUnderlyingBuilding = has (buildingSpace . buildingSpaceBuildings . traverse . to getBuildingType . filtered (== getUnderlyingBuilding buildingType)) playerData
        hasEnoughResources = isNonNegative $ playerData ^. playerResources ^-^ buildingCost buildingType
