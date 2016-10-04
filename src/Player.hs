{-# LANGUAGE TemplateHaskell #-}
module Player where

import Worker
import Building
import Workplace
import Resources

import Data.Map
import Control.Lens

newtype PlayerId = PlayerId Int deriving (Eq, Ord, Show)

data PlayerData = PlayerData {
  _playerId :: PlayerId,
  _workers :: Map WorkerId WorkerState,
  _buildingSpace :: BuildingSpace,
  _buildingOccupants :: BuildingOccupants,
  _playerStatus :: PlayerStatus,
  _playerResources :: Resources
} deriving (Show, Eq)

data PlayerStatus = MovingWorker |
                    Waiting |
                    OccupantsInvalid |
                    CuttingForest |
                    DiggingPassage |
                    DiggingCave |
                    MakingDecision DecisionType |
                    BuildingLivingRoom
  deriving (Show, Eq)

data DecisionType = WorkerNeedDecision WorkplaceId | CaveOrPassageDecision deriving (Show, Eq)

makeLenses ''PlayerData

allOccupants :: PlayerData -> [BuildingOccupant]
allOccupants plData = WorkerOccupant <$> keys (plData ^. workers)

verifyOccupants :: PlayerData -> [OccupantError]
verifyOccupants plData = areOccupantsValid (allOccupants plData) buildings occupants
  where buildings = plData ^. buildingSpace
        occupants = plData ^. buildingOccupants

checkOccupantsAfterTurn :: PlayerData -> PlayerData
checkOccupantsAfterTurn plData =
  let status = plData ^. playerStatus
      occupantsValid = Prelude.null $ verifyOccupants plData
  in if occupantsValid && status == OccupantsInvalid then set playerStatus Waiting plData else plData

stopTurn :: PlayerData -> PlayerData
stopTurn = checkOccupantsAfterTurn . set playerStatus OccupantsInvalid

applyAction :: WorkplaceId -> WorkplaceData -> PlayerData -> PlayerData
applyAction wpId workplaceData = over playerResources (assignResources workplaceData) . applySpecificAction wpId workplaceData
  where applySpecificAction _ (CutForest _) = set playerStatus CuttingForest
        applySpecificAction _ (DigPassage _) = set playerStatus DiggingPassage
        applySpecificAction _ (DigCave _) = set playerStatus (MakingDecision CaveOrPassageDecision)
        applySpecificAction workplaceId WorkerNeed = set playerStatus (MakingDecision $ WorkerNeedDecision workplaceId)
        applySpecificAction _ ResourceAddition = stopTurn

applyWorkplaceData :: WorkplaceData -> PlayerData -> PlayerData
applyWorkplaceData workplaceData = over playerResources (assignResources workplaceData)

initialPlayers :: Map PlayerId PlayerData
initialPlayers = fromList
  [(PlayerId i,
    PlayerData (PlayerId i) (createWorkers (i * 2 - 1) 2) initialBuildingSpace empty (if i == 1 then MovingWorker else Waiting) initialResources)
      | i <- [1..2]]
