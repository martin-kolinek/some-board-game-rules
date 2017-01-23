{-# LANGUAGE FlexibleContexts #-}
module Universe.Actions where

import Prelude hiding (lookup)
import Control.Monad.Except
import Control.Lens hiding (universe)
import Data.List (sortOn, groupBy, find)
import Data.Function (on)

import Universe.Player
import Universe.Workplace
import Universe.Building
import Player
import Decisions
import Actions
import Building
import Universe
import Util
import Worker
import Workplace

performSteps :: PlayerId -> Universe -> Universe
performSteps plId universe =
  case universe ^? players . ix plId . playerStatus of
    Just (PerformingAction workplaceId (PerformStep step continuation)) ->
      universe &
      advanceStatus plId workplaceId continuation &
      performStep step plId workplaceId &
      performSteps plId
    Just (PerformingAction _ ActionEnd) -> if null $ getOccupantErrors universe plId
      then universe &
           set (players . ix plId . playerStatus) Waiting &
           set (players . ixMaybe (nextPlayer universe plId) . playerStatus) MovingWorker
      else universe
    _ -> universe

advanceStatus :: PlayerId -> WorkplaceId -> ActionDefinition -> Universe -> Universe
advanceStatus plId workplaceId continuation =
      set (players . ix plId . playerStatus) (PerformingAction workplaceId continuation)

performStep :: ActionStep -> PlayerId -> WorkplaceId -> Universe -> Universe
performStep (AddResourcesStep _) _ _ = id
performStep (CollectResourcesStep _ _) _ _ = id
performStep AddWorkerStep _ _ = id
performStep SetStartPlayerStep _ _ = id

selectPosition :: MonadError String m => PlayerId -> Position -> Direction -> Universe -> m Universe
selectPosition plId pos dir universe =
  case universe ^? players . ix plId . playerStatus of
    Just (PerformingAction workplaceId (AwaitInteraction (BuildBuildingsInteraction _ buildings) continuation)) ->
      universe &
      advanceStatus plId workplaceId continuation &
      mapMOf (players . ix plId . buildingSpace) (buildNewBuildings pos dir buildings) <&>
      performSteps plId
    _ -> throwError "Currently not needing position"

cancelSelection :: MonadError String m => PlayerId -> Universe -> m Universe
cancelSelection plId universe =
  case universe ^? players . ix plId . playerStatus of
    Just (PerformingAction workplaceId (AwaitInteraction (BuildBuildingsInteraction CanCancelBuilding _) continuation)) ->
      universe &
      advanceStatus plId workplaceId continuation &
      performSteps plId &
      return
    _ -> throwError "Cannot cancel selection"

alterOccupants :: MonadError String m => PlayerId -> BuildingOccupants -> Universe -> m Universe
alterOccupants plId occupants universe = universe &
  set (players . ix plId . buildingSpace . buildingSpaceOccupants) occupants &
  performSteps plId &
  return

startWorking :: MonadError String m => PlayerId -> WorkerId -> WorkplaceId -> Universe -> m Universe
startWorking plId workerId workplaceId universe = do
  currentWorkplaceType <- checkMaybe "Invalid workplace" (universe ^? availableWorkplaces . ix workplaceId . workplaceType)
  check "Workplace already occupied" (workplaceId `elem` freeWorkplaces universe)
  playerData <- checkMaybe "Invalid player" (universe ^? players . ix plId)
  workerData <- checkMaybe "Invalid worker" (playerData ^? workers . ix workerId)
  check "Worker already working" $ hasn't (currentWorkplace . traverse) workerData
  return $ universe &
    players . ix plId .~ (playerData &
      playerStatus .~ PerformingAction workplaceId (workplaceAction currentWorkplaceType) &
      workers . ix workerId . currentWorkplace .~ Just workplaceId) &
    performSteps plId

finishTurn :: MonadError String m => Universe -> m Universe
finishTurn universe = do
  check "Not every player finished" $ hasn't (players . traverse . playerStatus . filtered (/= Waiting)) universe
  let startPlayer = universe ^. startingPlayer
  return $ universe &
    players . traverse . workers . traverse .~ initialWorkerState &
    players . traverse %~ collectCrops &
    players . ix startPlayer . playerStatus .~ MovingWorker

chooseOption :: MonadError String m => PlayerId -> Options -> Universe -> m Universe
chooseOption plId option universe = case (universe ^? players . ix plId . playerStatus) of
  Just (PerformingAction workplaceId (Decision options)) -> do
    (_, continuation) <- checkMaybe "Invalid option" $ find ((== option) . fst) options
    return $ universe &
      advanceStatus plId workplaceId continuation &
      performSteps plId
  _ -> throwError "Not needing an option"

type CropToPlant = (CropType, Position)

plantCrops :: MonadError String m => PlayerId -> [CropToPlant] -> Universe -> m Universe
plantCrops plId crops universe = do
  case universe ^? players . ix plId . playerStatus of
    Just (PerformingAction workplaceId (AwaitInteraction PlantCropsInteraction continuation)) -> do
      let groupedCrops = groupBy ((==) `on` fst) $ sortOn fst crops
      check "Too many crops" (all ((<=2) . length) groupedCrops)
      universe &
        mapMOf (players . ix plId . buildingSpace) (plantCropsInBuildingSpace crops) <&>
        advanceStatus plId workplaceId continuation <&>
        performSteps plId
    _ -> throwError "Not planting crops"
