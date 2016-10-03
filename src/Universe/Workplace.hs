{-# LANGUAGE FlexibleContexts #-}
module Universe.Workplace where

import Data.Map hiding ((\\))
import Data.List ((\\))
import Control.Lens hiding (universe)
import Control.Monad.Except
import Data.Maybe

import Universe
import Universe.Worker
import Workplace
import Worker
import Player
import Universe.Building
import Util

getWorkplaces :: Universe -> Map WorkplaceId WorkplaceData
getWorkplaces = view availableWorkplaces

getWorkplaceOccupants :: Universe -> WorkplaceId -> [WorkerId]
getWorkplaceOccupants universe workplace = [w | w <- toListOf (players . folding elems . workers . folding keys) universe, getWorkerWorkplace universe w == Just workplace]

freeWorkplaces :: Universe -> [WorkplaceId]
freeWorkplaces universe = universeAvailableWorkplaces \\ universeOccupiedWorkplaces
  where universeOccupiedWorkplaces = catMaybes $ view currentWorkplace <$> workerStates
        universeAvailableWorkplaces = keys $ view availableWorkplaces universe
        workerStates = toListOf (players . folding elems . workers . folding elems) universe

updateWorkplacesAfterTurn :: Universe -> Universe
updateWorkplacesAfterTurn = over (availableWorkplaces . traverse) updateWorkplaceAfterTurn

checkWorkplacePrecondition :: MonadError String m => Universe -> WorkplaceData -> m ()
checkWorkplacePrecondition universe WorkerNeed =
  check (currentPlayerCanBuildRoom universe || currentPlayerCanMakeChild universe) "No space for a child and no space for a building"
checkWorkplacePrecondition _ _ = return ()
