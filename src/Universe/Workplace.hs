{-# LANGUAGE FlexibleContexts #-}
module Universe.Workplace where

import Data.Map hiding ((\\))
import Data.List ((\\))
import Control.Lens hiding (universe)
import Data.Maybe
import Data.AdditiveGroup

import Universe
import Universe.Worker
import Workplace
import Worker
import Player
import Building
import Actions
import Resources

getWorkplaces :: Universe -> Map WorkplaceId WorkplaceData
getWorkplaces = view availableWorkplaces

getWorkplaceType :: WorkplaceData -> WorkplaceType
getWorkplaceType = view workplaceType

getWorkplaceResources :: WorkplaceData -> Resources
getWorkplaceResources = view workplaceStoredResources

getWorkplaceOccupants :: Universe -> WorkplaceId -> [WorkerId]
getWorkplaceOccupants universe workplace = [w | w <- toListOf (players . folding elems . workers . folding keys) universe, getWorkerWorkplace universe w == Just workplace]

freeWorkplaces :: Universe -> [WorkplaceId]
freeWorkplaces universe = universeAvailableWorkplaces \\ universeOccupiedWorkplaces
  where universeOccupiedWorkplaces = catMaybes $ view currentWorkplace <$> workerStates
        universeAvailableWorkplaces = keys $ view availableWorkplaces universe
        workerStates = toListOf (players . folding elems . workers . folding elems) universe

addWorkplaceResources :: WorkplaceData -> WorkplaceData
addWorkplaceResources workplaceData = workplaceData & workplaceStoredResources .~ newResources
  where newResources = if oldResources == zeroV then firstAddition else oldResources ^+^ normalAddition
        oldResources = workplaceData ^. workplaceStoredResources
        (normalAddition, firstAddition) = workplaceResourceAddition $ workplaceData ^. workplaceType

workplaceAction :: WorkplaceType -> ActionDefinition
workplaceAction CutForest = CompositeAction $
  ActionCombination AndOr
    (InteractionAction (BuildBuildingsInteraction [Grass, Field]) [])
    (InteractionAction CollectResourcesInteraction [])
workplaceAction DigPassage = CompositeAction $
  ActionCombination AndOr
    (InteractionAction (BuildBuildingsInteraction [Cave, Passage]) [])
    (InteractionAction CollectResourcesInteraction [])
workplaceAction DigCave = CompositeAction $
  ActionCombination AndOr
    (InteractionAction CollectResourcesInteraction [])
    (ActionCombination Or
      (InteractionAction (BuildBuildingsInteraction [Cave, Cave]) [])
      (InteractionAction (BuildBuildingsInteraction [Cave, Passage]) []))
workplaceAction WorkerNeed =
  CompositeAction $
    ActionCombination Or
      (InteractionAction HireWorkerInteraction [])
      (InteractionAction (BuildBuildingsInteraction [LivingRoom]) [])
workplaceAction ResourceAddition = StepsAction [AddResourcesStep (wood 1 ^+^ stone 1 ^+^ iron 1 ^+^ food 1 ^+^ money 2)]
workplaceAction GatherWood = StepsAction [CollectResourcesStep]
workplaceAction GatherFood = CompositeAction $
  ActionCombination AndOr
    (InteractionAction CollectResourcesInteraction [AddResourcesStep (wheat 1)])
    (InteractionAction (BuildBuildingsInteraction [Grass, Field]) [])
workplaceAction MakeStartPlayer =
  StepsAction [
    CollectResourcesStep,
    AddResourcesStep (iron 2),
    SetStartPlayerStep]
workplaceAction HouseWork = CompositeAction $
  ActionCombination AndOr
    (InteractionAction (BuildBuildingsInteraction [LivingRoom]) [])
    (InteractionAction CollectResourcesInteraction [AddDogStep])
workplaceAction Farming =
  CompositeAction $
    ActionCombination AndThenOr
      (InteractionAction (BuildBuildingsInteraction [Grass, Field]) [])
      (InteractionAction PlantCropsInteraction [])
workplaceAction WeaponMaking =
  CompositeAction $ InteractionAction ArmWorkerInteraction []
