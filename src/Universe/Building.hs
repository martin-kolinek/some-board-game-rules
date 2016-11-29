module Universe.Building where

import Control.Lens hiding (universe)
import Data.Map
import Data.Maybe

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

getPlayerPossibleOccupants :: PlayerData -> [BuildingOccupant]
getPlayerPossibleOccupants playerData =
  (DogOccupant <$> toListOf (playerAnimals . dogs . traverse) playerData) ++ (WorkerOccupant <$> toListOf (workers . folding keys) playerData)

currentPlayerCanBuildRoom :: Universe -> Bool
currentPlayerCanBuildRoom universe = has (currentPlayerData . buildingSpace . to getBuildings . traverse . filtered isCave) universe &&
  has (currentPlayerData . playerResources . woodAmount . filtered (>=4)) universe &&
  has (currentPlayerData . playerResources . stoneAmount . filtered (>=3)) universe

currentPlayerCanMakeChild :: Universe -> Bool
currentPlayerCanMakeChild = has (currentPlayerData . filtered playerCanMakeChild)
  where playerCanMakeChild playerData = canSupportAdditionalWorker (getPlayerPossibleOccupants playerData) (playerData ^. buildingSpace)
