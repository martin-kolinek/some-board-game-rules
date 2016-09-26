{-# LANGUAGE RankNTypes #-}
module Universe.Worker where

import Control.Lens hiding (universe)
import Data.Map
import Data.Maybe
import Control.Monad

import Util
import Universe
import Player
import Worker
import Workplace

getWorkers :: Universe -> PlayerId -> [WorkerId]
getWorkers universe player = toListOf (players . ix player . workers . folding keys) universe

getWorkerWorkplace :: Universe -> WorkerId -> Maybe WorkplaceId
getWorkerWorkplace universe workerId = universe ^? (workerState workerId . currentWorkplace . traverse)

getPlayerId :: Universe -> WorkerId -> Maybe PlayerId
getPlayerId universe workerId = listToMaybe $ do
  playerData <- elems $ view players universe
  guard $ member workerId $ playerData ^. workers
  return $ playerData ^. playerId

workerState :: WorkerId -> Traversal' Universe WorkerState
workerState workerId = byPlayerId . workers . ix workerId
  where playerDataByMaybe plId =  players . ixMaybe plId
        byPlayerId fres universe = playerDataByMaybe (getPlayerId universe workerId) fres universe

workerWorking :: WorkerState -> Bool
workerWorking = isJust . view currentWorkplace

newWorkerId :: Universe -> WorkerId
newWorkerId universe = WorkerId $ (maximum workerNumbers + 1)
  where getNumberFromId (WorkerId number) = number
        workerNumbers = toListOf (players . traverse . workers . to keys . traverse . to getNumberFromId) universe
