module Rules (Universe, WorkerId, WorkplaceId, WorkplaceAction(IncreaseScore),
                   getScore, getWorkers, getWorkplaces, getWorkerWorkplace, getWorkplaceOccupants,
                   initialUniverse, startWorking, finishTurn) where

import           Universe
