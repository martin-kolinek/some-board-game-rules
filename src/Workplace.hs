module Workplace where

newtype WorkplaceId = WorkplaceId Int deriving (Eq, Ord, Show)

data WorkplaceAction = IncreaseScore | CutForest deriving (Eq, Show)
