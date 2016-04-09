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

data Universe = Universe {
  _availableWorkplaces :: Map WorkplaceId WorkplaceAction,
  _players :: Map PlayerId PlayerData,
  _currentPlayer :: Maybe PlayerId
} deriving (Show)

newtype PlayerId = PlayerId Int deriving (Eq, Ord, Show)

data PlayerData = PlayerData {
  _playerId :: PlayerId,
  _workers :: Map WorkerId WorkerState,
  _score :: Int
} deriving (Show, Eq)

newtype WorkerId = WorkerId Int deriving (Eq, Ord, Show)

data WorkerState = WorkerState {
  _currentWorkplace :: Maybe WorkplaceId
} deriving (Show, Eq)

initialWorkerState = WorkerState Nothing

newtype WorkplaceId = WorkplaceId Int deriving (Eq, Ord, Show)

data WorkplaceAction = IncreaseScore deriving (Eq, Show)

makeLenses ''Universe
makeLenses ''WorkerState
makeLenses ''PlayerData

getCurrentPlayer :: Universe -> Maybe PlayerId
getCurrentPlayer = view currentPlayer

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

getPlayerId :: Universe -> WorkerId -> Maybe PlayerId
getPlayerId universe workerId = listToMaybe $ do
  playerData <- elems $ view players universe
  guard $ M.member workerId $ playerData ^. workers
  return $ playerData ^. playerId

currentPlayerData :: Traversal' Universe PlayerData
currentPlayerData fres universe = (players . fromMaybe ignored (ix <$> (universe ^. currentPlayer))) fres universe

nextPlayer :: Universe -> Maybe PlayerId
nextPlayer universe = do
  currentPlayer <- universe ^. currentPlayer
  let playerIds = keys (universe ^. players)
      hasFreeWorkers playerId = has (players . ix playerId . workers . folding M.elems . currentWorkplace . filtered isNothing) universe
      candidatePlayers = (tail . dropWhile (/= currentPlayer)) $ playerIds ++ playerIds
  listToMaybe $ filter hasFreeWorkers candidatePlayers

workerState :: WorkerId -> Traversal' Universe WorkerState
workerState workerId = byPlayerId . workers . ix workerId
  where playerDataByMaybe plId =  players . fromMaybe ignored (ix <$> plId)
        byPlayerId fres universe = playerDataByMaybe (getPlayerId universe workerId) fres universe

workerWorking :: WorkerState -> Bool
workerWorking = isJust . view currentWorkplace

freeWorkplaces :: Universe -> [WorkplaceId]
freeWorkplaces universe = universeAvailableWorkplaces \\ universeOccupiedWorkplaces
  where universeOccupiedWorkplaces = catMaybes $ view currentWorkplace <$> workerStates
        universeAvailableWorkplaces = keys $ view availableWorkplaces universe
        workerStates = toListOf (players . folding M.elems . workers . folding M.elems) universe

applyAction :: WorkplaceAction -> PlayerData -> PlayerData
applyAction IncreaseScore = over score (+1)

startWorking :: MonadError String m => WorkerId -> WorkplaceId -> Universe -> m Universe
startWorking workerId workplaceId universe = do
  check workerExists "Worker does not exist"
  check workerIdle "Worker already working"
  check workerBelongsToCurrentPlayer "Worker does not belong to current player"
  workplaceAction <- checkMaybe (workplaceId `M.lookup` view availableWorkplaces universe) "Workplace does not exist"
  check workplaceEmpty "Workplace occupied"
  let withAssignedWorker = over currentWorkerState setWorkplace universe
      withAppliedAction = over currentPlayerData (applyAction workplaceAction) withAssignedWorker
      withNextPlayer = set currentPlayer (nextPlayer withAppliedAction) withAppliedAction
  return withNextPlayer
  where currentWorkerState :: Traversal' Universe WorkerState
        currentWorkerState = workerState workerId
        workerExists = notNullOf currentWorkerState universe
        workerIdle = nullOf (currentWorkerState . currentWorkplace . traverse) universe
        workplaceEmpty = workplaceId `elem` freeWorkplaces universe
        setWorkplace = set currentWorkplace $ Just workplaceId
        workerBelongsToCurrentPlayer = fromMaybe False $ member workerId <$> universe ^? (currentPlayerData . workers)

createWorkplaces count = fromList [(WorkplaceId i, IncreaseScore) | i <- [0 .. count - 1]]

createWorkers initial count = fromList [(WorkerId i, initialWorkerState) | i <- [initial .. initial + count - 1]]

createPlayers numbersOfWorkers = fromList [(PlayerId i, PlayerData (PlayerId i) (createWorkers initial count) 0) | (i, count, initial) <- zip3 [0..] numbersOfWorkers (scanl (+) 0 numbersOfWorkers)]

initialUniverse = Universe (createWorkplaces 6) (createPlayers [2, 3]) (Just (PlayerId 0))

finishTurn :: MonadError String m => Universe -> m Universe
finishTurn universe = do
  check allWorkersBusy "Not all workers are busy"
  let withWorkersFreed = set (players . traverse . workers . traverse) initialWorkerState universe
  return $ set currentPlayer (listToMaybe $ getPlayers withWorkersFreed) withWorkersFreed
  where allWorkersBusy = isNothing $ universe ^. currentPlayer
