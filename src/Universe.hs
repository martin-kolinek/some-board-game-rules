{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Universe where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad
import           Data.Map.Strict as M hiding ((\\), filter)
import           Data.Maybe
import           Data.List
import           Prelude         hiding (lookup)
import           Control.Monad.Except
import           Util
import           Worker
import           Workplace
import           Building

data Universe = Universe {
  _availableWorkplaces :: Map WorkplaceId WorkplaceAction,
  _players :: Map PlayerId PlayerData
} deriving (Show)

newtype PlayerId = PlayerId Int deriving (Eq, Ord, Show)

data PlayerData = PlayerData {
  _playerId :: PlayerId,
  _workers :: Map WorkerId WorkerState,
  _buildingSpace :: BuildingSpace,
  _buildingOccupants :: BuildingOccupants,
  _score :: Int,
  _playerStatus :: PlayerStatus,
  _mostRecentWorker :: Maybe WorkerId
} deriving (Show, Eq)

data PlayerStatus = MovingWorker | Waiting | OccupantsInvalid | CuttingForest deriving (Show, Eq)

makeLenses ''Universe
makeLenses ''PlayerData

getCurrentPlayer :: Universe -> Maybe PlayerId
getCurrentPlayer universe =
  listToMaybe [view playerId player | player <- toListOf (players . traverse) universe, view playerStatus player /= Waiting]

getPlayers :: Universe -> [PlayerId]
getPlayers = toListOf (players . folding M.keys)

getScore :: Universe -> PlayerId -> Int
getScore universe playerId = fromMaybe 0 $ universe ^? (players . ix playerId . score)

getWorkers :: Universe -> PlayerId -> [WorkerId]
getWorkers universe playerId = toListOf (players . ix playerId . workers . folding M.keys) universe

getWorkplaces :: Universe -> Map WorkplaceId WorkplaceAction
getWorkplaces = view availableWorkplaces

getWorkerWorkplace :: Universe -> WorkerId -> Maybe WorkplaceId
getWorkerWorkplace universe workerId = universe ^? (workerState workerId . currentWorkplace . traverse)

getWorkplaceOccupants :: Universe -> WorkplaceId -> [WorkerId]
getWorkplaceOccupants universe workplace = [w | w <- toListOf (players . folding M.elems . workers . folding M.keys) universe, getWorkerWorkplace universe w == Just workplace]

getOccupantErrors :: Universe -> PlayerId -> [OccupantError]
getOccupantErrors universe playerId = toListOf (players . ix playerId . folding verifyOccupants) universe

getBuildingSpace :: Universe -> PlayerId -> [Building]
getBuildingSpace universe playerId = toListOf (players . ix playerId . buildingSpace . folding getBuildings) universe

getBuildingOccupants :: Universe -> PlayerId -> BuildingOccupants
getBuildingOccupants universe playerId = fromMaybe M.empty $ universe ^? (players . ix playerId . buildingOccupants)

getAllOccupants :: Universe -> PlayerId -> [BuildingOccupant]
getAllOccupants universe playerId = toListOf (players . ix playerId . folding getPlayerPossibleOccupants) universe

getPlayerPossibleOccupants :: PlayerData -> [BuildingOccupant]
getPlayerPossibleOccupants playerData = WorkerOccupant <$> toListOf (workers . folding keys) playerData

getPlayerStatus :: Universe -> PlayerId -> PlayerStatus
getPlayerStatus universe playerId = fromMaybe Waiting $ universe ^? (players . ix playerId . playerStatus)

getPlayerId :: Universe -> WorkerId -> Maybe PlayerId
getPlayerId universe workerId = listToMaybe $ do
  playerData <- elems $ view players universe
  guard $ M.member workerId $ playerData ^. workers
  return $ playerData ^. playerId

currentPlayerData :: Traversal' Universe PlayerData
currentPlayerData fres universe = (players . fromMaybe ignored (ix <$> getCurrentPlayer universe)) fres universe

nextPlayer :: Universe -> Maybe PlayerId
nextPlayer universe = do
  currentPlayer <- getCurrentPlayer universe
  let playerIds = keys (universe ^. players)
      hasFreeWorkers playerId = has (players . ix playerId . workers . folding M.elems . currentWorkplace . filtered isNothing) universe
      candidatePlayers = (tail . dropWhile (/= currentPlayer)) $ playerIds ++ playerIds
  listToMaybe $ filter hasFreeWorkers candidatePlayers

workerState :: WorkerId -> Traversal' Universe WorkerState
workerState workerId = byPlayerId . workers . ix workerId
  where playerDataByMaybe plId =  players . ixMaybe plId
        byPlayerId fres universe = playerDataByMaybe (getPlayerId universe workerId) fres universe

workerWorking :: WorkerState -> Bool
workerWorking = isJust . view currentWorkplace

freeWorkplaces :: Universe -> [WorkplaceId]
freeWorkplaces universe = universeAvailableWorkplaces \\ universeOccupiedWorkplaces
  where universeOccupiedWorkplaces = catMaybes $ view currentWorkplace <$> workerStates
        universeAvailableWorkplaces = keys $ view availableWorkplaces universe
        workerStates = toListOf (players . folding M.elems . workers . folding M.elems) universe

stopTurn :: PlayerData -> PlayerData
stopTurn = checkOccupantsAfterTurn . set playerStatus OccupantsInvalid . set mostRecentWorker Nothing

checkOccupantsAfterTurn :: PlayerData -> PlayerData
checkOccupantsAfterTurn plData =
  let status = plData ^. playerStatus
      occupantsValid = Prelude.null $ verifyOccupants plData
  in if occupantsValid && status == OccupantsInvalid then set playerStatus Waiting plData else plData

applyAction :: WorkplaceAction -> PlayerData -> PlayerData
applyAction IncreaseScore playerData = (stopTurn . over score (+1)) playerData
applyAction CutForest playerData = set playerStatus CuttingForest playerData

selectPosition :: MonadError String m => Position -> Direction -> Universe -> m Universe
selectPosition position direction universe = do
  let currentPlayerStatus = universe ^? (currentPlayerData . playerStatus)
  updatedUniverse <- mapMOf currentPlayerData (applyPosition currentPlayerStatus position direction) universe
  return $ startNextPlayer universe updatedUniverse

cancelSelection :: MonadError String m => Universe -> m Universe
cancelSelection universe =
  checkMaybe "Nothing to cancel" $ do
    currentPlayerStatus <- universe ^? (currentPlayerData . playerStatus)
    guard (currentPlayerStatus == CuttingForest)
    workerToRestore <- universe ^? (currentPlayerData . mostRecentWorker . traverse)
    let playerDataModification = set (workers . ix workerToRestore) (WorkerState Nothing) . set playerStatus MovingWorker . set mostRecentWorker Nothing
    return $ over currentPlayerData playerDataModification universe

applyPosition (Just CuttingForest) position direction plData = stopTurn <$> mapMOf buildingSpace (cutForest position direction) plData
applyPosition _ _ _ _ = throwError "Currently not needing position"

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

verifyOccupants :: PlayerData -> [OccupantError]
verifyOccupants plData = areOccupantsValid (allOccupants plData) buildings occupants
  where buildings = plData ^. buildingSpace
        occupants = plData ^. buildingOccupants

allOccupants :: PlayerData -> [BuildingOccupant]
allOccupants plData = WorkerOccupant <$> M.keys (plData ^. workers)

startWorking :: MonadError String m => WorkerId -> WorkplaceId -> Universe -> m Universe
startWorking workerId workplaceId universe = do
  check workerExists "Worker does not exist"
  check workerIdle "Worker already working"
  check workerBelongsToCurrentPlayer "Worker does not belong to current player"
  check currentPlayerCanMoveWorker "Current player cannot start working a worker now"
  workplaceAction <- checkMaybe "Workplace does not exist" (workplaceId `M.lookup` view availableWorkplaces universe)
  check workplaceEmpty "Workplace occupied"
  let withAssignedWorker = over currentWorkerState setWorkplace universe
      withLastWorker = set (currentPlayerData . mostRecentWorker) (Just workerId) withAssignedWorker
      withAppliedAction = over currentPlayerData (applyAction workplaceAction) withLastWorker
  return $ startNextPlayer withLastWorker withAppliedAction
  where currentWorkerState :: Traversal' Universe WorkerState
        currentWorkerState = workerState workerId
        workerExists = notNullOf currentWorkerState universe
        workerIdle = nullOf (currentWorkerState . currentWorkplace . traverse) universe
        workplaceEmpty = workplaceId `elem` freeWorkplaces universe
        setWorkplace = set currentWorkplace $ Just workplaceId
        workerBelongsToCurrentPlayer = fromMaybe False $ member workerId <$> universe ^? (currentPlayerData . workers)
        currentPlayerCanMoveWorker = has (currentPlayerData . playerStatus . filtered (MovingWorker ==)) universe

createWorkplaces count increaseScoreCount = fromList [(WorkplaceId i, if i < increaseScoreCount then IncreaseScore else CutForest) | i <- [0 .. count - 1]]

createWorkers initial count = fromList [(WorkerId i, initialWorkerState) | i <- [initial .. initial + count - 1]]

createPlayers numbersOfWorkers = fromList
  [(PlayerId i,
    PlayerData (PlayerId i) (createWorkers initial count) initialBuildingSpace M.empty 0 (if i == 0 then MovingWorker else Waiting) Nothing)
      | (i, count, initial) <- zip3 [0..] numbersOfWorkers (scanl (+) 0 numbersOfWorkers)]

initialUniverse =
  let withoutOccupants = Universe (createWorkplaces 12 6) (createPlayers [2, 3])
      assignInitialWorkers plData = set buildingOccupants (initialOccupants (allOccupants plData) (plData ^. buildingSpace)) plData
  in over (players . traverse) assignInitialWorkers withoutOccupants

finishTurn :: MonadError String m => Universe -> m Universe
finishTurn universe = do
  check allPlayersWaiting "Not every player finished"
  let withWorkersFreed = set (players . traverse . workers . traverse) initialWorkerState universe
  return $ set (taking 1 (players . traverse) . playerStatus) MovingWorker withWorkersFreed
  where allPlayersWaiting = hasn't (players . traverse . playerStatus . filtered (/= Waiting)) universe
