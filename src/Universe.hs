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
  _workers             :: Map WorkerId WorkerState,
  _score               :: Int
} deriving (Show)

newtype WorkerId = WorkerId Int deriving (Eq, Ord, Show)

data WorkerState = WorkerState {
  _currentWorkplace :: Maybe WorkplaceId
} deriving (Show)

initialWorkerState = WorkerState Nothing

newtype WorkplaceId = WorkplaceId Int deriving (Eq, Ord, Show)

data WorkplaceAction = IncreaseScore deriving (Eq, Show)

makeLenses ''Universe
makeLenses ''WorkerState

getScore :: Universe -> Int
getScore = view score

getWorkers :: Universe -> [WorkerId]
getWorkers = keys . view workers

getWorkplaces :: Universe -> Map WorkplaceId WorkplaceAction
getWorkplaces = view availableWorkplaces

getWorkerWorkplace :: Universe -> WorkerId -> Maybe WorkplaceId
getWorkerWorkplace universe workerId = do
  state <- workerId `M.lookup` view workers universe
  view currentWorkplace state

workerState :: WorkerId -> Lens' Universe (Maybe WorkerState)
workerState workerId = workers . at workerId

workerWorking :: WorkerState -> Bool
workerWorking = isJust . view currentWorkplace

freeWorkplaces :: Universe -> [WorkplaceId]
freeWorkplaces universe = universeAvailableWorkplaces \\ universeOccupiedWorkplaces
  where universeOccupiedWorkplaces = catMaybes $ view currentWorkplace <$> workerStates
        universeAvailableWorkplaces = keys $ view availableWorkplaces universe
        workerStates = elems (view workers universe)

check :: MonadError e m => Bool -> e -> m ()
check True _ = return ()
check False e = throwError e

checkMaybe :: MonadError e m => Maybe a -> e -> m a
checkMaybe Nothing e = throwError e
checkMaybe (Just x) _ = return x

applyAction :: WorkplaceAction -> Universe -> Universe
applyAction IncreaseScore = over score (+1)

startWorking :: MonadError String m => WorkerId -> WorkplaceId -> Universe -> m Universe
startWorking workerId workplaceId universe = do
  check workerExists "Worker does not exist"
  check workerIdle "Worker already working"
  workplaceAction <- checkMaybe (workplaceId `M.lookup` view availableWorkplaces universe) "Workplace does not exist"
  check workplaceEmpty "Workplace occupied"
  let withAssignedWorker = over currentWorkerState setWorkplace universe
  return $ applyAction workplaceAction withAssignedWorker
  where currentWorkerState :: Lens' Universe (Maybe WorkerState)
        currentWorkerState = workerState workerId
        workerExists = isJust $ view currentWorkerState universe
        workerIdle = isNothing $ do
          workState <- view currentWorkerState universe
          view currentWorkplace workState
        workplaceEmpty = workplaceId `elem` freeWorkplaces universe
        setWorkplace = liftM $ set currentWorkplace $ Just workplaceId

initialUniverse = Universe (fromList [(WorkplaceId 1, IncreaseScore)]) (fromList [(WorkerId 1, initialWorkerState)]) 0

finishTurn :: MonadError String m => Universe -> m Universe
finishTurn universe = do
  check allWorkersBusy "Not all workers are busy"
  return $ over workers (M.map (const initialWorkerState)) universe
  where allWorkersBusy = all workerWorking (elems (view workers universe))
