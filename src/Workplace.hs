module Workplace where

newtype WorkplaceId = WorkplaceId Int deriving (Eq, Ord, Show)

data WorkplaceAction = IncreaseScore deriving (Eq, Show)
