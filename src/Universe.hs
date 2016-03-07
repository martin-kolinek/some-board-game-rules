{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Universe where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad
import           Data.Map.Strict as M hiding ((\\))
import           Data.Maybe
import           Data.List
import           Prelude         hiding (lookup)
import           Control.Monad.Except

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

getScore :: Universe -> PlayerId -> Int
getScore universe playerId = fromMaybe 0 $ universe ^? (players . ix playerId . score)

getWorkers :: Universe -> [WorkerId]
getWorkers = toListOf (players . folding M.elems . workers . folding M.keys)

getWorkplaces :: Universe -> Map WorkplaceId WorkplaceAction
getWorkplaces = view availableWorkplaces

getWorkerWorkplace :: Universe -> WorkerId -> Maybe WorkplaceId
getWorkerWorkplace universe workerId = universe ^? (workerState workerId . currentWorkplace . traverse)

getWorkplaceOccupants :: Universe -> WorkplaceId -> [WorkerId]
getWorkplaceOccupants universe workplace = [w | w <- getWorkers universe, getWorkerWorkplace universe w == Just workplace]

getPlayerId :: Universe -> WorkerId -> Maybe PlayerId
getPlayerId universe workerId = listToMaybe $ do
  playerData <- elems $ view players universe
  guard $ M.member workerId $ playerData ^. workers
  return $ playerData ^. playerId

currentPlayerData :: Traversal' Universe PlayerData
currentPlayerData fres universe = (players . fromMaybe ignored (ix <$> (universe ^. currentPlayer))) fres universe

nextPlayer :: Universe -> Maybe PlayerId -> Maybe PlayerId
nextPlayer universe maybeCurrentPlayer = do
  currentPlayer <- maybeCurrentPlayer
  let playerIds = keys (universe ^. players)
      hasWorkers =
      cycled = cycle playerIds
  return undefined

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

check :: MonadError e m => Bool -> e -> m ()
check True _ = return ()
check False e = throwError e

checkMaybe :: MonadError e m => Maybe a -> e -> m a
checkMaybe Nothing e = throwError e
checkMaybe (Just x) _ = return x

applyAction :: WorkplaceAction -> PlayerData -> PlayerData
applyAction IncreaseScore = over score (+1)

startWorking :: MonadError String m => WorkerId -> WorkplaceId -> Universe -> m Universe
startWorking workerId workplaceId universe = do
  check workerExists "Worker does not exist"
  check workerIdle "Worker already working"
  workplaceAction <- checkMaybe (workplaceId `M.lookup` view availableWorkplaces universe) "Workplace does not exist"
  check workplaceEmpty "Workplace occupied"
  let withAssignedWorker = over currentWorkerState setWorkplace universe
      withAppliedAction = over currentPlayerData (applyAction workplaceAction) withAssignedWorker
  return undefined
  where currentWorkerState :: Traversal' Universe WorkerState
        currentWorkerState = workerState workerId
        workerExists = notNullOf currentWorkerState universe
        workerIdle = nullOf (currentWorkerState . currentWorkplace) universe
        workplaceEmpty = workplaceId `elem` freeWorkplaces universe
        setWorkplace = set currentWorkplace $ Just workplaceId

initialUniverse = Universe (fromList [(WorkplaceId 1, IncreaseScore), (WorkplaceId 2, IncreaseScore), (WorkplaceId 3, IncreaseScore)]) (fromList [(WorkerId 1, initialWorkerState), (WorkerId 2, initialWorkerState)]) 0

finishTurn :: MonadError String m => Universe -> m Universe
finishTurn universe = do
  check allWorkersBusy "Not all workers are busy"
  return $ over workers (M.map (const initialWorkerState)) universe
  where allWorkersBusy = all workerWorking (elems (view workers universe))
