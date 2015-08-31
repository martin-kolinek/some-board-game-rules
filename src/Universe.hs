{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Universe where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad
import           Data.Map.Strict
import           Data.Maybe
import           Prelude         hiding (lookup)

data Universe = Universe {
  _availableWorkplaces :: Map WorkplaceId WorkplaceAction,
  _workers             :: Map WorkerId WorkerState,
  _score               :: Int
}

newtype WorkerId = WorkerId Int deriving (Eq, Ord)

data WorkerState = WorkerState {
  _currentWorkplace :: Maybe WorkplaceId
}

newtype WorkplaceId = WorkplaceId Int deriving (Eq, Ord)

data WorkplaceAction = IncreaseScore

makeLenses ''Universe
makeLenses ''WorkerState

workerState :: WorkerId -> Lens' Universe (Maybe WorkerState)
workerState workerId = workers . at workerId

startWorking :: WorkerId -> WorkplaceId -> Universe -> Universe
startWorking workerId workplaceId universe =
  if workerExists && workerIdle && workplaceEmpty
    then over currentWorkerState (liftM $ set currentWorkplace $ Just workplaceId) universe
    else universe
  where currentWorkerState :: Lens' Universe (Maybe WorkerState)
        currentWorkerState = workerState workerId
        workerExists = isJust $ view currentWorkerState universe
        workerIdle = isNothing $ do
          workState <- view currentWorkerState universe
          view currentWorkplace workState
        workplaceEmpty =
