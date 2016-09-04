module Universe.Building where

import Control.Lens
import Data.Map
import Data.Maybe
import Control.Monad.Error.Class

import Universe
import Player
import Building

getOccupantErrors :: Universe -> PlayerId -> [OccupantError]
getOccupantErrors universe playerId = toListOf (players . ix playerId . folding verifyOccupants) universe

getBuildingSpace :: Universe -> PlayerId -> [Building]
getBuildingSpace universe playerId = toListOf (players . ix playerId . buildingSpace . folding getBuildings) universe

getBuildingOccupants :: Universe -> PlayerId -> BuildingOccupants
getBuildingOccupants universe playerId = fromMaybe empty $ universe ^? (players . ix playerId . buildingOccupants)

getAllOccupants :: Universe -> PlayerId -> [BuildingOccupant]
getAllOccupants universe playerId = toListOf (players . ix playerId . folding getPlayerPossibleOccupants) universe

getPlayerPossibleOccupants :: PlayerData -> [BuildingOccupant]
getPlayerPossibleOccupants playerData = WorkerOccupant <$> toListOf (workers . folding keys) playerData

