{-# LANGUAGE TemplateHaskell #-}
module Workplace where

import Resources

import Control.Lens
import Data.Map.Strict
import Data.AdditiveGroup

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

workplaceResourceAddition :: WorkplaceType -> (Resources, Resources)
workplaceResourceAddition CutForest = (wood 1, wood 1)
workplaceResourceAddition DigPassage = (stone 1, stone 1)
workplaceResourceAddition DigCave = (stone 1, stone 1)
workplaceResourceAddition GatherWood = (wood 1, wood 1)
workplaceResourceAddition GatherFood = (food 1, food 1)
workplaceResourceAddition MakeStartPlayer = (food 1, food 1)
workplaceResourceAddition _ = (zeroV, zeroV)
