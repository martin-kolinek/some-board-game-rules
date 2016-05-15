module Rules (
                Universe,
                WorkerId,
                WorkplaceId,
                PlayerId,
                WorkplaceAction(IncreaseScore),
                BuildingOccupant(WorkerOccupant),
                getPlayers,
                getCurrentPlayer,
                getScore,
                getWorkers,
                getWorkplaces,
                getWorkerWorkplace,
                getWorkplaceOccupants,
                getBuildingSpace,
                getBuildingOccupants,
                getAllOccupants,
                getOccupantErrors,
                initialUniverse,
                startWorking,
                finishTurn,
                alterOccupants) where

import           Universe
import           Worker
import           Workplace
import           Building
