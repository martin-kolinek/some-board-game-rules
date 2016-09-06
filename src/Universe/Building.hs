module Universe.Building where

import Control.Lens hiding (universe)
import Data.Map
import Data.Maybe

import Universe
import Player
import Building

getOccupantErrors :: Universe -> PlayerId -> [OccupantError]
getOccupantErrors universe player = toListOf (players . ix player . folding verifyOccupants) universe

getBuildingSpace :: Universe -> PlayerId -> [Building]
getBuildingSpace universe player = toListOf (players . ix player . buildingSpace . folding getBuildings) universe

getBuildingOccupants :: Universe -> PlayerId -> BuildingOccupants
getBuildingOccupants universe player = fromMaybe empty $ universe ^? (players . ix player . buildingOccupants)

getAllOccupants :: Universe -> PlayerId -> [BuildingOccupant]
getAllOccupants universe player = toListOf (players . ix player . folding getPlayerPossibleOccupants) universe

getPlayerPossibleOccupants :: PlayerData -> [BuildingOccupant]
getPlayerPossibleOccupants playerData = WorkerOccupant <$> toListOf (workers . folding keys) playerData

