{-# LANGUAGE FlexibleContexts #-}
module Universe.Actions where

import Prelude hiding (lookup)
import Control.Monad.Except
import Control.Lens hiding (universe)
import Data.Map
import Data.Maybe

import Universe.Player
import Universe.Worker
import Universe.Workplace
import Player
import Building
import Universe
import Util
import Worker
import Workplace

selectPosition :: MonadError String m => Position -> Direction -> Universe -> m Universe
selectPosition position direction universe = do
  let currentPlayerStatus = universe ^? (currentPlayerData . playerStatus)
  check (has currentPlayerData universe) "Currently not needing position"
  playerUpdated <- mapMOf currentPlayerData (applyPosition currentPlayerStatus position direction) universe
  return $ startNextPlayer universe playerUpdated

applyPosition :: MonadError String m => Maybe PlayerStatus -> Position -> Direction -> PlayerData -> m PlayerData
applyPosition (Just CuttingForest) position direction plData = stopTurn <$> mapMOf buildingSpace (cutForest position direction) plData
applyPosition (Just DiggingPassage) position direction plData = stopTurn <$> mapMOf buildingSpace (digPassage position direction) plData
applyPosition (Just DiggingCave) position direction plData = stopTurn <$> mapMOf buildingSpace (digCave position direction) plData
applyPosition (Just BuildingLivingRoom) position _ plData = stopTurn <$> mapMOf buildingSpace (buildLivingRoom position) plData
applyPosition _ _ _ _ = throwError "Currently not needing position"

cancelSelection :: MonadError String m => Universe -> m Universe
cancelSelection universe =
  checkMaybe "Nothing to cancel" $ do
    currentPlayerStatus <- universe ^? (currentPlayerData . playerStatus)
    guard (currentPlayerStatus `elem` [CuttingForest, DiggingPassage, DiggingCave, BuildingLivingRoom])
    let universeWithPlayerWaiting = set (currentPlayerData . playerStatus) Waiting universe
    return $ startNextPlayer universe universeWithPlayerWaiting

alterOccupants :: MonadError String m => PlayerId -> BuildingOccupants -> Universe -> m Universe
alterOccupants player occupants universe =
  let withNewOccupants = set (players . ix player . buildingOccupants) occupants universe
      withCheckedOccupants = over (players . ix player) checkOccupantsAfterTurn withNewOccupants
  in return $ startNextPlayer universe withCheckedOccupants

startNextPlayer :: Universe -> Universe -> Universe
startNextPlayer originalUniverse updatedUniverse =
  let changePlayer = hasn't (currentPlayerData . playerStatus . filtered (/= Waiting)) updatedUniverse
      withNextPlayer = set (players . ixMaybe (nextPlayer originalUniverse) . playerStatus) MovingWorker updatedUniverse
  in if changePlayer then withNextPlayer else updatedUniverse

startWorking :: MonadError String m => WorkerId -> WorkplaceId -> Universe -> m Universe
startWorking workerId workplaceId universe = do
  check workerExists "Worker does not exist"
  check workerIdle "Worker already working"
  check workerBelongsToCurrentPlayer "Worker does not belong to current player"
  check currentPlayerCanMoveWorker "Current player cannot start working a worker now"
  workplaceAction <- checkMaybe "Workplace does not exist" (workplaceId `lookup` view availableWorkplaces universe)
  check workplaceEmpty "Workplace occupied"
  let withAssignedWorker = over currentWorkerState setWorkplace universe
      withAppliedAction = over currentPlayerData (applyAction workplaceAction) withAssignedWorker
      withClearedWorkplace = over (availableWorkplaces . ix workplaceId) clearWorkspace withAppliedAction
  return $ startNextPlayer withAssignedWorker withClearedWorkplace
  where currentWorkerState :: Traversal' Universe WorkerState
        currentWorkerState = workerState workerId
        workerExists = notNullOf currentWorkerState universe
        workerIdle = nullOf (currentWorkerState . currentWorkplace . traverse) universe
        workplaceEmpty = workplaceId `elem` freeWorkplaces universe
        setWorkplace = set currentWorkplace $ Just workplaceId
        workerBelongsToCurrentPlayer = fromMaybe False $ member workerId <$> universe ^? (currentPlayerData . workers)
        currentPlayerCanMoveWorker = has (currentPlayerData . playerStatus . filtered (MovingWorker ==)) universe

finishTurn :: MonadError String m => Universe -> m Universe
finishTurn universe = do
  let allPlayersWaiting = hasn't (players . traverse . playerStatus . filtered (/= Waiting)) universe
  check allPlayersWaiting "Not every player finished"
  let withWorkersFreed = set (players . traverse . workers . traverse) initialWorkerState universe
      withFirstPlayerMovingWorker = set (taking 1 (players . traverse) . playerStatus) MovingWorker withWorkersFreed
      withUpdatedWorkplaces = updateWorkplacesAfterTurn withFirstPlayerMovingWorker
  return withUpdatedWorkplaces

chooseChildDesireOption :: MonadError String m => ChildDesireOptions -> Universe -> m Universe
chooseChildDesireOption option universe = do
  let correctStatus = has (currentPlayerData . playerStatus . filtered (==ChoosingChildDesireOption)) universe
  check correctStatus "Not possible to select that now"
  let nextUniverse = case option of
                       MakeChild -> over currentPlayerData stopTurn universe
                       BuildRoom -> set (currentPlayerData . playerStatus) BuildingLivingRoom universe
  return $ startNextPlayer universe nextUniverse
