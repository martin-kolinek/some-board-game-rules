{-# LANGUAGE FlexibleContexts #-}
module Universe.Actions where

import Prelude hiding (lookup)
import Control.Monad.Except
import Control.Lens hiding (universe)
import Data.List (sortOn, groupBy)
import Data.Map
import Data.Maybe
import Data.Function (on)

import Universe.Player
import Universe.Worker
import Universe.Workplace
import Universe.Building
import Player
import Decisions
import Building
import Universe
import Util
import Worker
import Workplace
import Resources

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
    guard (currentPlayerStatus `elem` [CuttingForest, DiggingPassage, DiggingCave])
    let universeWithPlayerWaiting = over currentPlayerData stopTurn universe
    return $ startNextPlayer universe universeWithPlayerWaiting

alterOccupants :: MonadError String m => PlayerId -> BuildingOccupants -> Universe -> m Universe
alterOccupants player occupants universe =
  let withNewOccupants = set (players . ix player . buildingSpace . buildingSpaceOccupants) occupants universe
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
  checkWorkplacePrecondition universe workplaceAction
  let withAssignedWorker = over currentWorkerState setWorkplace universe
      withAppliedAction = applyAction workplaceId workplaceAction withAssignedWorker
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
      withFirstPlayerMovingWorker = set (players . ix (universe ^. startingPlayer) . playerStatus) MovingWorker withWorkersFreed
      withUpdatedWorkplaces = updateWorkplacesAfterTurn withFirstPlayerMovingWorker
  return withUpdatedWorkplaces

chooseOption :: MonadError String m => Options -> Universe -> m Universe
chooseOption option universe = do
  currentPlayerStatus <- checkMaybe "Not currently making decision" $ universe ^? (currentPlayerData . playerStatus)
  let findDecisionType (MakingDecision x) = Just x
      findDecisionType _ = Nothing
  decisionType <- checkMaybe "Not currently making decision" $ findDecisionType currentPlayerStatus
  nextUniverse <- chooseOptionWithDecisionType decisionType option universe
  return $ startNextPlayer universe nextUniverse

chooseOptionWithDecisionType :: MonadError [Char] m => DecisionType -> Options -> Universe -> m Universe
chooseOptionWithDecisionType (WorkerNeedDecision workplaceId) (WorkerNeedOption option) = chooseWorkerNeedOption workplaceId option
chooseOptionWithDecisionType CaveOrPassageDecision (CaveOrPassageOption option) = chooseCaveOrPassage option
chooseOptionWithDecisionType AnyRoomDecision (AnyRoomOption option) = chooseAnyRoom option
chooseOptionWithDecisionType _ _ = const $ throwError "Not a valid decision for current state"

chooseWorkerNeedOption :: MonadError [Char] m => WorkplaceId -> WorkerNeedOptions -> Universe -> m Universe
chooseWorkerNeedOption workplaceId HireWorker universe = do
  check (currentPlayerCanMakeChild universe) "No room for a child"
  let workerId = newWorkerId universe
      addWorker = insert workerId (WorkerState $ Just workplaceId)
      alterPlayerOccupants plData = set (buildingSpace . buildingSpaceOccupants) (findSpaceForWorker (plData ^. buildingSpace) (WorkerOccupant workerId)) plData
  return $ over currentPlayerData (stopTurn . alterPlayerOccupants . over workers addWorker) universe
chooseWorkerNeedOption _ BuildRoom universe = do
  check (currentPlayerCanBuildRoom universe) "Unable to build a room"
  let setStatus = set (currentPlayerData . playerStatus) BuildingLivingRoom
      resourceTraversal :: Traversal' Universe Resources
      resourceTraversal = currentPlayerData . playerResources
      removeResources = over (resourceTraversal . woodAmount) (subtract 4) . over (resourceTraversal . stoneAmount) (subtract 3)
  return $ (setStatus . removeResources) universe

chooseCaveOrPassage :: MonadError String m => CaveOrPassageOptions -> Universe -> m Universe
chooseCaveOrPassage ChooseCave = return . set (currentPlayerData . playerStatus) DiggingCave
chooseCaveOrPassage ChoosePassage = return . set (currentPlayerData . playerStatus) DiggingPassage
chooseCaveOrPassage NoDigging = return . over currentPlayerData stopTurn

chooseAnyRoom :: MonadError String m => AnyRoomOptions -> Universe -> m Universe
chooseAnyRoom ChooseNoRoom = return . over currentPlayerData stopTurn
chooseAnyRoom ChooseLivingRoom = return . set (currentPlayerData . playerStatus) BuildingLivingRoom

type CropToPlant = (CropType, Position)

plantCrops :: MonadError String m => [CropToPlant] -> Universe -> m Universe
plantCrops crops universe = do
  let groupedCrops = groupBy ((==) `on` fst) $ sortOn fst crops
      plantingPlayerTraversal :: Traversal' Universe PlayerData
      plantingPlayerTraversal = players . traverse . filtered (has $ playerStatus . filtered (== PlantingCrops))
  playerData <- checkMaybe "Not currently planting crops" $ universe ^? plantingPlayerTraversal
  check (has plantingPlayerTraversal universe) "Not currently planting crops"
  check (all ((<=2) . length) groupedCrops) "Too many crops"
  updatedBuildingSpace <- foldM (flip $ uncurry plantCrop) (playerData ^. buildingSpace) crops
  let withUpdatedBuildingSpace = set (plantingPlayerTraversal . buildingSpace) updatedBuildingSpace universe
      withStoppedTurn = over plantingPlayerTraversal stopTurn withUpdatedBuildingSpace
  return $ startNextPlayer universe withStoppedTurn
