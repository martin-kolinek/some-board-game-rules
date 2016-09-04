module Workplace where

newtype WorkplaceId = WorkplaceId Int deriving (Eq, Ord, Show)

data WorkplaceAction = CutForest deriving (Eq, Show)
