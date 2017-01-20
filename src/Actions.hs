{-# LANGUAGE TemplateHaskell #-}
module Actions where

import Building
import Decisions

import Control.Lens.TH

data ActionInteraction =
  BuildBuildingsInteraction BuildingCancellation [BuildingType] |
  PlantCropsInteraction
  deriving (Show, Eq)

data ActionStep =
  AddResourcesStep |
  AddWorkerStep |
  SetStartPlayerStep
  deriving (Show, Eq)

data ActionDefinition =
  ActionEnd |
  PerformStep { _actionStep :: ActionStep, _actionContinuation :: ActionDefinition} |
  AwaitInteraction { _actionInteraction :: ActionInteraction, _actionContinuation :: ActionDefinition }|
  Decision { _actionOptions :: [(Options, ActionDefinition)]}
  deriving (Show, Eq)

makeLenses ''ActionDefinition
