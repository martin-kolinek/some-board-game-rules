{-# LANGUAGE TemplateHaskell #-}
module Player where

import Worker
import Building
import Workplace
import Resources

import Data.Map hiding (foldl')
import Control.Lens
import Data.List (sort, group, foldl')
import Data.List.NonEmpty (NonEmpty(..))

newtype PlayerId = PlayerId Int deriving (Eq, Ord, Show)

data PlayerData = PlayerData {
  _playerId :: PlayerId,
  _workers :: Map WorkerId WorkerState,
  _buildingSpace :: BuildingSpace,
  _playerStatus :: PlayerStatus,
  _playerResources :: Resources,
  _playerAnimals :: Animals
} deriving (Show, Eq)

data PlayerStatus = MovingWorker |
                    Waiting |
                    OccupantsInvalid |
                    PerformingAction (NonEmpty PlayerStatusDuringAction)
  deriving (Show, Eq)

data PlayerStatusDuringAction =
                    CuttingForest |
                    DiggingPassage |
                    DiggingCave |
                    MakingDecision DecisionType |
                    BuildingLivingRoom |
                    PlantingCrops
  deriving (Show, Eq)

data DecisionType = WorkerNeedDecision WorkplaceId | CaveOrPassageDecision | AnyRoomDecision deriving (Show, Eq)

makeLenses ''PlayerData

allOccupants :: PlayerData -> [BuildingOccupant]
allOccupants plData = (WorkerOccupant <$> keys (plData ^. workers)) ++ (DogOccupant <$> (plData ^. playerAnimals . dogs))

verifyOccupants :: PlayerData -> [OccupantError]
verifyOccupants plData = areOccupantsValid (allOccupants plData) buildings
  where buildings = plData ^. buildingSpace

checkOccupantsAfterTurn :: PlayerData -> PlayerData
checkOccupantsAfterTurn plData =
  let status = plData ^. playerStatus
      occupantsValid = Prelude.null $ verifyOccupants plData
  in if occupantsValid && status == OccupantsInvalid then set playerStatus Waiting plData else plData

stopTurn :: PlayerData -> PlayerData
stopTurn = checkOccupantsAfterTurn . set playerStatus OccupantsInvalid

collectCrops :: PlayerData -> PlayerData
collectCrops playerData =
  let grouped = group $ sort $ toListOf (buildingSpace . buildingSpaceCrops . traverse . to plantedCropType) playerData
      addResource plData cropGroup = over (playerResources . cropResource (head cropGroup)) (+ (length cropGroup)) plData
      withAddedResources = foldl' addResource playerData grouped
      removeCrop (Just (PlantedCrop _ 1)) = Nothing
      removeCrop (Just (PlantedCrop tp n)) = Just (PlantedCrop tp (n-1))
      removeCrop _ = Nothing
      removeCropAt pos = over (buildingSpace . buildingSpaceCrops . at pos) removeCrop
      plantedCropType (PlantedCrop tp _) = tp
  in foldl' (flip removeCropAt) withAddedResources availableBuildingPositions

initialPlayers :: Map PlayerId PlayerData
initialPlayers = fromList
  [(PlayerId i,
    PlayerData
     (PlayerId i)
     initialWorkers
     (initialBuildingSpace $ WorkerOccupant <$> keys initialWorkers)
     (if i == 1 then MovingWorker else Waiting)
     initialResources
     initialAnimals)
  | i <- [1..2], let initialWorkers = createWorkers (i * 2 - 1) 2]

getCurrentActionStatus :: PlayerStatus -> Maybe PlayerStatusDuringAction
getCurrentActionStatus (PerformingAction (status :| _)) = Just status
getCurrentActionStatus _ = Nothing

createSimpleStatus :: PlayerStatusDuringAction -> PlayerStatus
createSimpleStatus actionStatus = PerformingAction (actionStatus :| [])
