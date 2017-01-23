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
  HouseWork |
  Farming deriving (Eq, Show)


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
assignResources (CutForest wd) = over woodAmount (+wd)
assignResources (DigPassage st) = over stoneAmount (+st)
assignResources (DigCave st) = over stoneAmount (+st)
assignResources ResourceAddition =
  over stoneAmount (+1) .
  over woodAmount (+1) .
  over ironAmount (+1) .
  over foodAmount (+1) .
  over moneyAmount (+2)
assignResources (GatherWood wd) = over woodAmount (+wd)
assignResources (GatherFood fd)= over foodAmount (+fd) . over wheatAmount (+1)
assignResources (MakeStartPlayer fd) = over foodAmount (+fd) . over ironAmount (+2)
assignResources WorkerNeed = id
assignResources HouseWork = id
assignResources Farming = id

initialWorkplaces :: Map WorkplaceId WorkplaceData
initialWorkplaces = fromList $ zip workplaceIds updatedWorkplaceDatas
  where workplaceDatas = replicate 8 (CutForest 0) ++ replicate 8 (DigCave 0) ++ replicate 8 (DigPassage 0) ++ replicate 4 WorkerNeed
        updatedWorkplaceDatas = updateWorkplaceAfterTurn <$> workplaceDatas
        workplaceIds = WorkplaceId <$> [1..]
