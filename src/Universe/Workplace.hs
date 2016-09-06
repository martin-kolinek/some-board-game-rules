module Universe.Workplace where

import Data.Map hiding ((\\))
import Data.List ((\\))
import Control.Lens hiding (universe)
import Data.Maybe

import Universe
import Universe.Worker
import Workplace
import Worker
import Player

getWorkplaces :: Universe -> Map WorkplaceId WorkplaceAction
getWorkplaces = view availableWorkplaces

getWorkplaceOccupants :: Universe -> WorkplaceId -> [WorkerId]
getWorkplaceOccupants universe workplace = [w | w <- toListOf (players . folding elems . workers . folding keys) universe, getWorkerWorkplace universe w == Just workplace]

freeWorkplaces :: Universe -> [WorkplaceId]
freeWorkplaces universe = universeAvailableWorkplaces \\ universeOccupiedWorkplaces
  where universeOccupiedWorkplaces = catMaybes $ view currentWorkplace <$> workerStates
        universeAvailableWorkplaces = keys $ view availableWorkplaces universe
        workerStates = toListOf (players . folding elems . workers . folding elems) universe
