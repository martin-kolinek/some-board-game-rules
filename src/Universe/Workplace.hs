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
import Universe.Player
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

applyAction :: WorkplaceId -> WorkplaceData -> Universe -> Universe
applyAction wpId workplaceData = applySpecificAction wpId workplaceData . over (currentPlayerData . playerResources) (assignResources workplaceData)
  where applySpecificAction _ (CutForest _) = set (currentPlayerData . playerStatus) CuttingForest
        applySpecificAction _ (DigPassage _) = set (currentPlayerData . playerStatus) DiggingPassage
        applySpecificAction _ (DigCave _) = set (currentPlayerData . playerStatus) (MakingDecision CaveOrPassageDecision)
        applySpecificAction workplaceId WorkerNeed = set (currentPlayerData . playerStatus) (MakingDecision $ WorkerNeedDecision workplaceId)
        applySpecificAction _ ResourceAddition = over currentPlayerData stopTurn
        applySpecificAction _ (GatherWood _) = over currentPlayerData stopTurn
        applySpecificAction _ (GatherFood _) = set (currentPlayerData . playerStatus) CuttingForest
        applySpecificAction _ (MakeStartPlayer _) = over currentPlayerData stopTurn . setStartingPlayer
          where setStartingPlayer universe = set startingPlayer nextStartingPlayer universe
                  where firstPlayer = head $ keys $ universe ^. players
                        nextStartingPlayer = fromMaybe firstPlayer $ getCurrentPlayer universe
        applySpecificAction _ HouseWork = (set (currentPlayerData . playerStatus) (MakingDecision AnyRoomDecision)) . addDogToCurrentPlayer
          where addDogToCurrentPlayer universe = over currentPlayerData (addDog universe) universe
        applySpecificAction _ Farming = set (currentPlayerData . playerStatus) CuttingForest

applyWorkplaceData :: WorkplaceData -> PlayerData -> PlayerData
applyWorkplaceData workplaceData = over playerResources (assignResources workplaceData)
