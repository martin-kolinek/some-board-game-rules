{-# LANGUAGE TemplateHaskell #-}
module Actions where

import Building
import Decisions
import Resources
import Worker

import Control.Lens.TH

data ActionInteraction =
  BuildBuildingsInteraction BuildingCancellation [BuildingType] |
  PlantCropsInteraction
  deriving (Show, Eq)

data ActionStep =
  AddResourcesStep Resources |
  CollectResourcesStep Resources Resources |
  AddWorkerStep |
  SetStartPlayerStep |
  AddDogStep |
  ArmWorkerStep WorkerStrength
  deriving (Show, Eq)

data ActionDefinition =
  ActionEnd |
  PerformStep { _actionStep :: ActionStep, _actionContinuation :: ActionDefinition} |
  AwaitInteraction { _actionInteraction :: ActionInteraction, _actionContinuation :: ActionDefinition }|
  Decision { _actionOptions :: [(Options, ActionDefinition)]}
  deriving (Show, Eq)

makeLenses ''ActionDefinition

armDecision :: ActionDefinition -> ActionDefinition
armDecision continuation = Decision $ [(ArmOption $ ArmWorker strength, PerformStep (ArmWorkerStep strength) continuation) | strength <- [1..8]] ++ [(ArmOption NoArming, continuation)]
