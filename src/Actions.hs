{-# LANGUAGE TemplateHaskell #-}
module Actions where

import Building
import Resources

import Control.Lens.TH
import Control.Lens

data ActionInteraction =
  BuildBuildingsInteraction [BuildingType] |
  PlantCropsInteraction |
  HireWorkerInteraction |
  CollectResourcesInteraction |
  ArmWorkerInteraction |
  AdventureInteraction
  deriving (Show, Eq)

data ActionStep =
  AddResourcesStep Resources |
  SetStartPlayerStep |
  AddDogStep |
  CollectResourcesStep |
  PayResources Resources
  deriving (Show, Eq)

data ActionDefinition =
  StepsAction [ActionStep] |
  CompositeAction CompositeActionDefinition
  deriving (Show, Eq)

data ActionCombinationType = AndThen | AndThenOr | Or | AndOr deriving (Show, Eq)

data CompositeActionDefinition =
  ActionCombination ActionCombinationType CompositeActionDefinition CompositeActionDefinition |
  InteractionAction ActionInteraction [ActionStep] |
  OptionalAction CompositeActionDefinition
  deriving (Show, Eq)

makeLenses ''ActionDefinition

possibleInteractions :: CompositeActionDefinition -> [ActionInteraction]
possibleInteractions composite = possibleInteractionsFromComposite composite
  where possibleInteractionsFromComposite (InteractionAction interaction _) = [interaction]
        possibleInteractionsFromComposite (ActionCombination AndThen first _) = possibleInteractionsFromComposite first
        possibleInteractionsFromComposite (ActionCombination _ first second) = possibleInteractionsFromComposite first ++ possibleInteractionsFromComposite second
        possibleInteractionsFromComposite (OptionalAction act) = possibleInteractionsFromComposite act

possibleInteractionsTraversal :: Fold CompositeActionDefinition ActionInteraction
possibleInteractionsTraversal = to possibleInteractions . traverse

data ActionAfterInteractionResult = InvalidInteraction | ActionFinished [ActionStep] | RemainingAction CompositeActionDefinition [ActionStep]

actionAfterInteraction :: CompositeActionDefinition -> ActionInteraction -> ActionAfterInteractionResult
actionAfterInteraction (InteractionAction interaction steps) performedInteraction = if interaction == performedInteraction then ActionFinished steps else InvalidInteraction
actionAfterInteraction (OptionalAction action) performedInteraction = case actionAfterInteraction action performedInteraction of
  InvalidInteraction -> InvalidInteraction
  ActionFinished steps -> ActionFinished steps
  RemainingAction act steps -> RemainingAction act steps
actionAfterInteraction (ActionCombination combinationType first second) performedInteraction =
  --yeah, this needs more thinking
  let combine Or (ActionFinished steps) _ _ _ = ActionFinished steps
      combine Or _ _ (ActionFinished steps) _ = ActionFinished steps
      combine Or (RemainingAction act steps) _ _ _ = RemainingAction act steps
      combine Or _ _ (RemainingAction act steps) _ = RemainingAction act steps
      combine AndThen (ActionFinished steps) _ _ act2 = RemainingAction act2 steps
      combine AndThen InvalidInteraction _ _ _ = InvalidInteraction
      combine AndThen (RemainingAction act1 steps) _ _ act2 = RemainingAction (ActionCombination AndThen act1 act2) steps
      combine AndOr (ActionFinished steps) _ _ act2 = RemainingAction (OptionalAction act2) steps
      combine AndOr _ act1 (ActionFinished steps) _ = RemainingAction (OptionalAction act1) steps
      combine AndOr (RemainingAction act1 steps) _ _ act2 = RemainingAction (ActionCombination AndOr act1 act2) steps
      combine AndOr _ act1 (RemainingAction act2 steps) _ = RemainingAction (ActionCombination AndOr act1 act2) steps
      combine AndThenOr (ActionFinished steps) _ _ act2 = RemainingAction (OptionalAction act2) steps
      combine AndThenOr _ _ (ActionFinished steps) _ = ActionFinished steps
      combine AndThenOr (RemainingAction act1 steps) _ _ act2 = RemainingAction (ActionCombination AndThen act1 (OptionalAction act2)) steps
      combine AndThenOr InvalidInteraction _ (RemainingAction act2 steps) _ = RemainingAction act2 steps
      combine _ InvalidInteraction _ InvalidInteraction _ = InvalidInteraction
  in combine combinationType (actionAfterInteraction first performedInteraction) first (actionAfterInteraction second performedInteraction) second
