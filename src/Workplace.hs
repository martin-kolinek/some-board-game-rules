{-# LANGUAGE TemplateHaskell #-}
module Workplace where

import Resources

import Control.Lens
import Data.Map.Strict

newtype WorkplaceId = WorkplaceId Int deriving (Eq, Ord, Show)

data WorkplaceData = WorkplaceData { _workplaceType :: WorkplaceType, _workplaceStoredResources :: Resources } deriving (Show, Eq)

data WorkplaceType =
  CutForest |
  DigPassage |
  DigCave |
  WorkerNeed |
  ResourceAddition |
  GatherWood |
  GatherFood |
  MakeStartPlayer |
  HouseWork |
  Farming |
  WeaponMaking deriving (Eq, Show)

makeLenses ''WorkplaceData

initialWorkplaces :: Map WorkplaceId WorkplaceData
initialWorkplaces = fromList $ zip workplaceIds workplaceDatas
  where workplaceTypes = replicate 8 CutForest ++ replicate 8 DigCave ++ replicate 8 DigPassage ++ replicate 4 WorkerNeed
        workplaceDatas = flip WorkplaceData initialResources <$> workplaceTypes
        workplaceIds = WorkplaceId <$> [1..]
