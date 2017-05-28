{-# LANGUAGE FlexibleContexts #-}
module Universe.Interaction where

import Prelude hiding (lookup)
import Control.Monad.Except
import Control.Lens hiding (universe)
import Data.List (sortOn, groupBy, foldl')
import Data.Function (on)
import Data.AdditiveGroup
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

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
buildBuildings plId pos dir buildings = performInteraction plId (BuildBuildingsInteraction buildings) $
  const $ const $ mapMOf (players . ix plId . buildingSpace) (buildNewBuildings pos dir buildings)

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
  checkOccupants universe plId
  let currentWorkplaceAction = workplaceAction currentWorkplaceType
  check "Precondition not met" $ actionPrecondition plId workerId workplaceId universe currentWorkplaceAction
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
plantCrops plId crops = performInteraction plId PlantCropsInteraction $ const $ const $ \universe -> do
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
collectResources plId = performInteraction plId CollectResourcesInteraction $ \workplaceId _ universe ->
  return $ performStep CollectResourcesStep plId workplaceId universe

finishAction :: MonadError String m => PlayerId -> Universe -> m Universe
finishAction plId universe = do
  check "Cannot finish action right now" $ canFinishAction universe plId
  return $ startNextPlayer plId universe

hireWorker :: MonadError String m => PlayerId -> Universe -> m Universe
hireWorker plId = performInteraction plId HireWorkerInteraction $ \workplaceId _ universe ->
  let addedWorkerId = newWorkerId universe
  in return $ universe &
       players . ix plId . workers . at addedWorkerId . non initialWorkerState . currentWorkplace .~ Just workplaceId &
       players . ix plId . buildingSpace %~ findSpaceForWorker (WorkerOccupant addedWorkerId)

armWorker :: MonadError String m => PlayerId -> Int -> Universe -> m Universe
armWorker plId amount = performInteraction plId ArmWorkerInteraction $ \_ workerId universe -> do
  check "Cannot arm with more than 8" $ amount <= 8
  check "Need to arm with at least 1" $ amount >= 1
  check "Not enough iron" $ all (amount <=) $ toListOf (players . ix plId . playerResources . ironAmount) universe
  return $ universe &
    players . ix plId . workers . ix workerId . workerStrength +~ amount &
    players . ix plId . playerResources . ironAmount -~ amount

performInteraction :: MonadError String m => PlayerId -> ActionInteraction -> (WorkplaceId -> WorkerId -> Universe -> m Universe) -> Universe -> m Universe
performInteraction plId interaction effect universe = do
  (workplaceId, playerStatusAction) <- checkMaybe "Not current player" $ universe ^? players . ix plId . playerStatus . statusActionAndWorkplace
  checkOccupants universe plId
  workerId <- checkMaybe "This shouldn't happen" -- TODO find a way to represent it in the type system
    $ universe ^? players . traverse . workers . to M.toList . traverse . filtered (has $ _2 . inWorkplace workplaceId) . _1
  check "Precondition not met" $ interactionPrecondition interaction workerId plId workplaceId universe
  case actionAfterInteraction playerStatusAction interaction of
    InvalidInteraction -> throwError ("Not possible right now " ++ show interaction)
    ActionFinished steps ->
      universe &
        effect workplaceId workerId <&>
        performSteps steps plId workplaceId <&>
        startNextPlayer plId
    RemainingAction act steps ->
      universe &
        effect workplaceId workerId <&>
        performSteps steps plId workplaceId <&>
        players . ix plId . playerStatus .~ PerformingAction workplaceId act

checkOccupants :: MonadError String m => Universe -> PlayerId -> m ()
checkOccupants universe plId = check "Fix occupants first" $ null $ getOccupantErrors universe plId
