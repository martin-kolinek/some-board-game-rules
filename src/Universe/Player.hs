module Universe.Player where

import Control.Lens hiding (universe)
import Data.Maybe
import Data.Map hiding (filter, null)
import Data.List (intersect, null)

import Player
import Universe
import Worker
import Resources
import Building

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

nextPlayer :: Universe -> Maybe PlayerId
nextPlayer universe = do
  currentPlayer <- getCurrentPlayer universe
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
canCancelBuilding universe plId = not $ null $ (universe ^.. (players . ix plId . playerStatus . to getCurrentActionStatus . traverse)) `intersect`
  [CuttingForest, DiggingCave, DiggingPassage, BuildingLivingRoom]

currentlyBuiltBuildings :: Universe -> PlayerId -> [BuildingType]
currentlyBuiltBuildings universe plId = case (universe ^? players . ix plId . playerStatus . to getCurrentActionStatus . traverse) of
  Just CuttingForest -> [Grass, Field]
  Just DiggingCave -> [Cave, Cave]
  Just DiggingPassage -> [Cave, Passage]
  Just BuildingLivingRoom -> [LivingRoom]
  _ -> []

isPlantingCrops :: Universe -> PlayerId -> Bool
isPlantingCrops universe plId = universe ^? (players . ix plId . playerStatus . to getCurrentActionStatus . traverse) == Just PlantingCrops

isMovingWorker :: Universe -> PlayerId -> Bool
isMovingWorker universe plId = universe ^? (players . ix plId . playerStatus) == Just MovingWorker
