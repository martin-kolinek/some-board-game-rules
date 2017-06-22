module Universe.Player where

import Control.Lens hiding (universe)
import Data.Maybe
import Data.Map hiding (filter, null)
import Control.Monad (guard)

import Player
import Universe
import Worker
import Resources
import Building
import Actions
import Util
import Universe.Workplace
import Universe.Actions

getCurrentPlayer :: Universe -> Maybe PlayerId
getCurrentPlayer universe =
  listToMaybe [view playerId player | player <- toListOf (players . traverse) universe, view playerStatus player /= Waiting]

getPlayers :: Universe -> [PlayerId]
getPlayers = toListOf (players . folding keys)

getPlayerStatus :: Universe -> PlayerId -> PlayerStatus
getPlayerStatus universe player = fromMaybe Waiting $ universe ^? (players . ix player . playerStatus)

getPlayerResources :: Universe -> PlayerId -> Resources
getPlayerResources universe player = fromMaybe initialResources $ universe ^? (players . ix player . playerResources)

nextPlayer :: Universe -> PlayerId -> Maybe PlayerId
nextPlayer universe currentPlayer = do
  let playerIds = keys (universe ^. players)
      hasFreeWorkers player = has (players . ix player . workers . folding elems . currentWorkplace . filtered isNothing) universe
      candidatePlayers = (tail . dropWhile (/= currentPlayer)) $ playerIds ++ playerIds
  listToMaybe $ filter hasFreeWorkers candidatePlayers

getStartingPlayer :: Universe -> PlayerId
getStartingPlayer universe = universe ^. startingPlayer

getAnimals :: Universe -> PlayerId -> [Animal]
getAnimals universe plId = toListOf (players . ix plId . playerAnimals . traverse) universe

currentlyBuiltBuildings :: Universe -> PlayerId -> [BuildingDescription]
currentlyBuiltBuildings universe plId = universe ^.. players . ix plId . playerStatus . statusAction . possibleInteractionsTraversal . to builtBuildings . traverse
  where builtBuildings (BuildBuildingsInteraction buildings) = [buildings]
        builtBuildings _ = []

isInteractionPossible :: (ActionInteraction -> Bool) -> Universe -> PlayerId -> Bool
isInteractionPossible interactionFunc universe plId = (not . null) $ do
  (workplace, action) <- universe ^.. players . ix plId . playerStatus . statusActionAndWorkplace
  interaction <- action ^.. possibleInteractionsTraversal . filtered interactionFunc
  worker <- getWorkplaceOccupants universe workplace
  guard $ interactionPrecondition worker workplace plId universe interaction
  return ()

isPlantingCrops :: Universe -> PlayerId -> Bool
isPlantingCrops = isInteractionPossible (== PlantCropsInteraction)

canCollectResources :: Universe -> PlayerId -> Bool
canCollectResources = isInteractionPossible (== CollectResourcesInteraction)

canHireWorker :: Universe -> PlayerId -> Bool
canHireWorker = isInteractionPossible (== HireWorkerInteraction)

canFinishAction :: Universe -> PlayerId -> Bool
canFinishAction universe plId =
  case universe ^? players . ix plId . playerStatus . statusAction of
    Just (OptionalAction _) -> True
    _ -> False

isMovingWorker :: Universe -> PlayerId -> Bool
isMovingWorker universe plId = universe ^? (players . ix plId . playerStatus) == Just MovingWorker

isArmingWorker :: Universe -> PlayerId -> Bool
isArmingWorker = isInteractionPossible (== ArmWorkerInteraction)

canGoOnAdventure :: Universe -> PlayerId -> Bool
canGoOnAdventure = isInteractionPossible (== AdventureInteraction)

startNextPlayer :: PlayerId -> Universe -> Universe
startNextPlayer plId universe = universe &
  players . ix plId . playerStatus .~ Waiting &
  players . ixMaybe (nextPlayer universe plId) . playerStatus .~ MovingWorker
