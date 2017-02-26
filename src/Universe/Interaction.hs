{-# LANGUAGE FlexibleContexts #-}
module Universe.Interaction where

import Prelude hiding (lookup)
import Control.Monad.Except
import Control.Lens hiding (universe)
import Data.List (sortOn, groupBy, foldl')
import Data.Function (on)
import Data.AdditiveGroup
import Data.Maybe (fromMaybe)

import Universe.Workplace
import Universe.Actions
import Universe.Building
import Universe.Player
import Universe.Worker
import Player
import Actions
import Building
import Universe
import Util
import Worker
import Workplace
import Resources

buildBuildings :: MonadError String m => PlayerId -> Position -> Direction -> [BuildingType] -> Universe -> m Universe
buildBuildings plId pos dir buildings = performInteraction plId (BuildBuildingsInteraction CanCancelBuilding buildings) $
  const $ mapMOf (players . ix plId . buildingSpace) (buildNewBuildings pos dir buildings)

alterOccupants :: MonadError String m => PlayerId -> BuildingOccupants -> Universe -> m Universe
alterOccupants plId occupants universe = return $ universe &
  players . ix plId . buildingSpace . buildingSpaceOccupants .~ occupants

startWorking :: MonadError String m => PlayerId -> WorkerId -> WorkplaceId -> Universe -> m Universe
startWorking plId workerId workplaceId universe = do
  currentWorkplaceType <- checkMaybe "Invalid workplace" (universe ^? availableWorkplaces . ix workplaceId . workplaceType)
  check "Workplace already occupied" (workplaceId `elem` freeWorkplaces universe)
  playerData <- checkMaybe "Invalid player" (universe ^? players . ix plId)
  workerData <- checkMaybe "Invalid worker" (playerData ^? workers . ix workerId)
  check "Worker already working" $ hasn't (currentWorkplace . traverse) workerData
  check "Not moving worker" (has (players . ix plId . playerStatus . filtered (== MovingWorker)) universe)
  let currentWorkplaceAction = workplaceAction currentWorkplaceType
  check "Precondition not met" $ actionPrecondition plId workplaceId universe currentWorkplaceAction
  let universeWithMovedWorker = universe & players . ix plId . workers . ix workerId . currentWorkplace .~ Just workplaceId
  case currentWorkplaceAction of
    StepsAction steps -> return $ universeWithMovedWorker &
      performSteps steps plId workplaceId &
      startNextPlayer plId
    CompositeAction composite -> return $ universeWithMovedWorker &
      players . ix plId . playerStatus .~ PerformingAction workplaceId composite

finishTurn :: MonadError String m => Universe -> m Universe
finishTurn universe = do
  check "Not every player finished" $ hasn't (players . traverse . playerStatus . filtered (/= Waiting)) universe
  let startPlayer = universe ^. startingPlayer
  return $ universe &
    players . traverse . workers . traverse .~ initialWorkerState &
    players . traverse %~ collectCrops &
    players . ix startPlayer . playerStatus .~ MovingWorker &
    availableWorkplaces . traverse %~ addWorkplaceResources

type CropToPlant = (CropType, Position)

plantCrops :: MonadError String m => PlayerId -> [CropToPlant] -> Universe -> m Universe
plantCrops plId crops = performInteraction plId PlantCropsInteraction $ const $ \universe -> do
  let groupedCrops = groupBy ((==) `on` fst) $ sortOn fst crops
      oldPlayerResources = fromMaybe zeroV $ universe ^? players . ix plId . playerResources
      subtractGroupResources resources grp = resources & cropResource (fst (head grp)) -~ length grp
      newPlayerResources = foldl' subtractGroupResources oldPlayerResources groupedCrops
  check "Too many crops" (all ((<=2) . length) groupedCrops)
  check "Not enough crops" $ isNonNegative newPlayerResources
  universe &
    mapMOf (players . ix plId . buildingSpace) (plantCropsInBuildingSpace crops) <&>
    players . ix plId . playerResources .~ newPlayerResources

collectResources :: MonadError String m => PlayerId -> Universe -> m Universe
collectResources plId = performInteraction plId CollectResourcesInteraction $ \workplaceId universe ->
  return $ performStep CollectResourcesStep plId workplaceId universe

finishAction :: MonadError String m => PlayerId -> Universe -> m Universe
finishAction plId universe = do
  case universe ^? players . ix plId . playerStatus . statusAction of
    Just (OptionalAction _) -> return $ startNextPlayer plId universe
    _ -> throwError "Cannot finish action right now"

hireWorker :: MonadError String m => PlayerId -> Universe -> m Universe
hireWorker plId = performInteraction plId HireWorkerInteraction $ \workplaceId universe ->
  let addedWorkerId = newWorkerId universe
  in return $ universe &
       players . ix plId . workers . at addedWorkerId . non initialWorkerState . currentWorkplace .~ Just workplaceId &
       players . ix plId . buildingSpace %~ findSpaceForWorker (WorkerOccupant addedWorkerId)

performInteraction :: MonadError String m => PlayerId -> ActionInteraction -> (WorkplaceId -> Universe -> m Universe) -> Universe -> m Universe
performInteraction plId interaction effect universe = do
  (workplaceId, playerStatusAction) <- checkMaybe "Not current player" $ universe ^? players . ix plId . playerStatus . statusActionAndWorkplace
  check "Fix occupants first" $ null $ getOccupantErrors universe plId
  case actionAfterInteraction playerStatusAction interaction of
    InvalidInteraction -> throwError "Not possible right now"
    ActionFinished steps ->
      universe &
        effect workplaceId <&>
        performSteps steps plId workplaceId <&>
        startNextPlayer plId
    RemainingAction act steps ->
      universe &
        effect workplaceId <&>
        performSteps steps plId workplaceId <&>
        players . ix plId . playerStatus .~ PerformingAction workplaceId act
