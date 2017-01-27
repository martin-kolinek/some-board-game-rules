{-# LANGUAGE RankNTypes #-}
module Universe.Worker where

import Control.Lens hiding (universe)
import Data.Map
import Data.Maybe

import Universe
import Player
import Worker
import Workplace

getWorkers :: Universe -> PlayerId -> [WorkerId]
getWorkers universe player = toListOf (players . ix player . workers . folding keys) universe

getWorkerWorkplace :: Universe -> WorkerId -> Maybe WorkplaceId
getWorkerWorkplace universe workerId = universe ^? (players . traverse . workers . ix workerId . currentWorkplace . traverse)

getWorkerStrength :: Universe -> WorkerId -> WorkerStrength
getWorkerStrength universe workerId = fromMaybe 0 $ universe ^? (players . traverse . workers . ix workerId . workerStrength)

workerWorking :: WorkerState -> Bool
workerWorking = isJust . view currentWorkplace

newWorkerId :: Universe -> WorkerId
newWorkerId universe = WorkerId (maximum workerNumbers + 1)
  where getNumberFromId (WorkerId number) = number
        workerNumbers = toListOf (players . traverse . workers . to keys . traverse . to getNumberFromId) universe
