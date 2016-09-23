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

data PlayerStatus = MovingWorker | Waiting | OccupantsInvalid | CuttingForest | DiggingPassage | DiggingCave deriving (Show, Eq)

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

applyAction :: WorkplaceData -> PlayerData -> PlayerData
applyAction workplaceData@(CutForest _) = over playerResources (assignResources workplaceData) . set playerStatus CuttingForest
applyAction workplaceData@(DigPassage _) = applyWorkplaceData workplaceData . set playerStatus DiggingPassage
applyAction workplaceData@(DigCave _) = applyWorkplaceData workplaceData . set playerStatus DiggingCave
applyAction workplaceData@ChildDesire = applyWorkplaceData workplaceData . stopTurn

applyWorkplaceData :: WorkplaceData -> PlayerData -> PlayerData
applyWorkplaceData workplaceData = over playerResources (assignResources workplaceData)
