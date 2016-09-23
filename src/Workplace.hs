module Workplace where

import Resources
import Control.Lens

newtype WorkplaceId = WorkplaceId Int deriving (Eq, Ord, Show)

data WorkplaceData = CutForest Int | DigPassage Int | DigCave Int deriving (Eq, Show)

updateWorkplaceAfterTurn :: WorkplaceData -> WorkplaceData
updateWorkplaceAfterTurn (CutForest 0) = CutForest 3
updateWorkplaceAfterTurn (CutForest x) = CutForest (x + 1)
updateWorkplaceAfterTurn (DigPassage x) = DigPassage (x + 1)
updateWorkplaceAfterTurn (DigCave x) = DigCave (x + 1)

clearWorkspace :: WorkplaceData -> WorkplaceData
clearWorkspace (CutForest _) = CutForest 0
clearWorkspace (DigPassage _) = DigPassage 0
clearWorkspace (DigCave _) = DigCave 0

assignResources :: WorkplaceData -> Resources -> Resources
assignResources (CutForest wood) = over woodAmount (+wood)
assignResources (DigPassage stone) = over stoneAmount (+stone)
assignResources (DigCave stone) = over stoneAmount (+stone)
