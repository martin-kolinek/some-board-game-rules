module Rules (Universe, WorkerId, WorkplaceId, WorkplaceAction(IncreaseScore),
                   getScore, getWorkers, getWorkplaces, getWorkerWorkplace,
                   initialUniverse, startWorking, finishTurn) where

import           Universe
