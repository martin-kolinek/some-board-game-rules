module Workplace where

import Resources

import Control.Lens
import Data.Map.Strict

newtype WorkplaceId = WorkplaceId Int deriving (Eq, Ord, Show)

data WorkplaceData =
  CutForest Int |
  DigPassage Int |
  DigCave Int |
  WorkerNeed |
  ResourceAddition |
  GatherWood Int |
  GatherFood Int |
  MakeStartPlayer Int |
  HouseWork deriving (Eq, Show)

data WorkerNeedOptions = HireWorker | BuildRoom deriving (Eq, Show)

data CaveOrPassageOptions = ChooseCave | ChoosePassage | NoDigging deriving (Eq, Show)

data Options =
  WorkerNeedOption WorkerNeedOptions |
  CaveOrPassageOption CaveOrPassageOptions deriving (Eq, Show)

updateWorkplaceAfterTurn :: WorkplaceData -> WorkplaceData
updateWorkplaceAfterTurn (CutForest 0) = CutForest 3
updateWorkplaceAfterTurn (CutForest x) = CutForest (x + 1)
updateWorkplaceAfterTurn (DigPassage x) = DigPassage (x + 1)
updateWorkplaceAfterTurn (DigCave x) = DigCave (x + 1)
updateWorkplaceAfterTurn (GatherWood x) = GatherWood (x + 1)
updateWorkplaceAfterTurn (GatherFood x) = GatherFood (x + 1)
updateWorkplaceAfterTurn (MakeStartPlayer x) = MakeStartPlayer (x + 1)
updateWorkplaceAfterTurn x = x

clearWorkspace :: WorkplaceData -> WorkplaceData
clearWorkspace (CutForest _) = CutForest 0
clearWorkspace (DigPassage _) = DigPassage 0
clearWorkspace (DigCave _) = DigCave 0
clearWorkspace (GatherWood _) = GatherWood 0
clearWorkspace (GatherFood _) = GatherFood 0
clearWorkspace (MakeStartPlayer _) = MakeStartPlayer 0
clearWorkspace x = x

assignResources :: WorkplaceData -> Resources -> Resources
assignResources (CutForest wood) = over woodAmount (+wood)
assignResources (DigPassage stone) = over stoneAmount (+stone)
assignResources (DigCave stone) = over stoneAmount (+stone)
assignResources ResourceAddition =
  over stoneAmount (+1) .
  over woodAmount (+1) .
  over ironAmount (+1) .
  over foodAmount (+1) .
  over goldAmount (+1)
assignResources (GatherWood wood) = over woodAmount (+wood)
assignResources (GatherFood food)= over foodAmount (+food) . over wheatAmount (+1)
assignResources (MakeStartPlayer food) = over foodAmount (+food) . over ironAmount (+2)
assignResources WorkerNeed = id
assignResources HouseWork = id

initialWorkplaces :: Map WorkplaceId WorkplaceData
initialWorkplaces = fromList $ zip workplaceIds updatedWorkplaceDatas
  where workplaceDatas = replicate 8 (CutForest 0) ++ replicate 8 (DigCave 0) ++ replicate 8 (DigPassage 0) ++ replicate 4 WorkerNeed
        updatedWorkplaceDatas = updateWorkplaceAfterTurn <$> workplaceDatas
        workplaceIds = WorkplaceId <$> [1..]
