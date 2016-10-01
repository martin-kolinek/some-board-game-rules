module Workplace where

import Resources

import Control.Lens
import Data.Map.Strict

newtype WorkplaceId = WorkplaceId Int deriving (Eq, Ord, Show)

data WorkplaceData = CutForest Int | DigPassage Int | DigCave Int | ChildDesire deriving (Eq, Show)

data ChildDesireOptions = MakeChild | BuildRoom

updateWorkplaceAfterTurn :: WorkplaceData -> WorkplaceData
updateWorkplaceAfterTurn (CutForest 0) = CutForest 3
updateWorkplaceAfterTurn (CutForest x) = CutForest (x + 1)
updateWorkplaceAfterTurn (DigPassage x) = DigPassage (x + 1)
updateWorkplaceAfterTurn (DigCave x) = DigCave (x + 1)
updateWorkplaceAfterTurn ChildDesire = ChildDesire

clearWorkspace :: WorkplaceData -> WorkplaceData
clearWorkspace (CutForest _) = CutForest 0
clearWorkspace (DigPassage _) = DigPassage 0
clearWorkspace (DigCave _) = DigCave 0
clearWorkspace ChildDesire = ChildDesire

assignResources :: WorkplaceData -> Resources -> Resources
assignResources (CutForest wood) = over woodAmount (+wood)
assignResources (DigPassage stone) = over stoneAmount (+stone)
assignResources (DigCave stone) = over stoneAmount (+stone)
assignResources ChildDesire = id

initialWorkplaces :: Map WorkplaceId WorkplaceData
initialWorkplaces = fromList $ zip workplaceIds updatedWorkplaceDatas
  where workplaceDatas = replicate 8 (CutForest 0) ++ replicate 8 (DigCave 0) ++ replicate 8 (DigPassage 0) ++ replicate 4 ChildDesire
        updatedWorkplaceDatas = updateWorkplaceAfterTurn <$> workplaceDatas
        workplaceIds = WorkplaceId <$> [1..]
