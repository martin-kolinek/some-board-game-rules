module Rules (
                Universe,
                WorkerId,
                WorkplaceId,
                PlayerId,
                WorkplaceAction(IncreaseScore),
                getPlayers,
                getScore,
                getWorkers,
                getWorkplaces,
                getWorkerWorkplace,
                getWorkplaceOccupants,
                initialUniverse,
                startWorking,
                finishTurn) where

import           Universe
