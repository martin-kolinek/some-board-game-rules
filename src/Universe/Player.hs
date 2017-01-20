module Universe.Player where

import Control.Lens hiding (universe)
import Data.Maybe
import Data.Map hiding (filter, null)

import Player
import Universe
import Worker
import Resources
import Building
import Actions

getCurrentPlayer :: Universe -> Maybe PlayerId
getCurrentPlayer universe =
  listToMaybe [view playerId player | player <- toListOf (players . traverse) universe, view playerStatus player /= Waiting]

getPlayers :: Universe -> [PlayerId]
getPlayers = toListOf (players . folding keys)

getPlayerStatus :: Universe -> PlayerId -> PlayerStatus
getPlayerStatus universe player = fromMaybe Waiting $ universe ^? (players . ix player . playerStatus)

getPlayerResources :: Universe -> PlayerId -> Resources
getPlayerResources universe player = fromMaybe initialResources $ universe ^? (players . ix player . playerResources)

currentPlayerData :: Traversal' Universe PlayerData
currentPlayerData fres universe = (players . fromMaybe ignored (ix <$> getCurrentPlayer universe)) fres universe

nextPlayer :: Universe -> PlayerId -> Maybe PlayerId
nextPlayer universe currentPlayer = do
  let playerIds = keys (universe ^. players)
      hasFreeWorkers player = has (players . ix player . workers . folding elems . currentWorkplace . filtered isNothing) universe
      candidatePlayers = (tail . dropWhile (/= currentPlayer)) $ playerIds ++ playerIds
  listToMaybe $ filter hasFreeWorkers candidatePlayers

getStartingPlayer :: Universe -> PlayerId
getStartingPlayer universe = universe ^. startingPlayer

getDogs :: Universe -> PlayerId -> [DogId]
getDogs universe plId = toListOf (players . ix plId . playerAnimals . dogs . traverse) universe

newDogId :: Universe -> DogId
newDogId universe = DogId (maximum dogNumbers + 1)
  where getNumberFromId (DogId number) = number
        dogNumbers = 0 : toListOf (players . traverse . playerAnimals . dogs . traverse . to getNumberFromId) universe

addDog :: Universe -> PlayerData -> PlayerData
addDog universe = over (buildingSpace . buildingSpaceOccupants) addDogToOccupants . over (playerAnimals . dogs) (dogId :)
  where dogId = newDogId universe
        addDogToOccupants occupants = alter (Just . (DogOccupant dogId :) . fromMaybe []) (0, 0) occupants

canCancelBuilding :: Universe -> PlayerId -> Bool
canCancelBuilding universe plId = case (universe ^? players . ix plId . playerStatus . statusAction . actionInteraction) of
  Just (BuildBuildingsInteraction CanCancelBuilding _) -> True
  _ -> False

currentlyBuiltBuildings :: Universe -> PlayerId -> [BuildingType]
currentlyBuiltBuildings universe plId = case (universe ^? players . ix plId . playerStatus . statusAction . actionInteraction) of
  Just (BuildBuildingsInteraction _ buildings) -> buildings
  _ -> []

isPlantingCrops :: Universe -> PlayerId -> Bool
isPlantingCrops universe plId = case universe ^? (players . ix plId . playerStatus . statusAction . actionInteraction) of
  Just PlantCropsInteraction -> True
  _ -> False

isMovingWorker :: Universe -> PlayerId -> Bool
isMovingWorker universe plId = universe ^? (players . ix plId . playerStatus) == Just MovingWorker
