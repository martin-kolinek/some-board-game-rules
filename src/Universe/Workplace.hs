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
import Decisions
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
  where newResources = if oldResources == zeroV then bonusAddition else oldResources ^+^ normalAddition
        oldResources = workplaceData ^. workplaceStoredResources
        (normalAddition, bonusAddition) = workplaceStoredResourcesAddition $ workplaceAction $ workplaceData ^. workplaceType
        workplaceStoredResourcesAddition (PerformStep (CollectResourcesStep normal bonus) continuation) = workplaceStoredResourcesAddition continuation ^+^ (normal, bonus)
        workplaceStoredResourcesAddition (PerformStep _ continuation) = workplaceStoredResourcesAddition continuation
        workplaceStoredResourcesAddition (AwaitInteraction _ continuation) = workplaceStoredResourcesAddition continuation
        workplaceStoredResourcesAddition (Decision options) = sumV $ workplaceStoredResourcesAddition <$> snd <$> options
        workplaceStoredResourcesAddition ActionEnd = zeroV

workplaceAction :: WorkplaceType -> ActionDefinition
workplaceAction CutForest =
  PerformStep (CollectResourcesStep (wood 1) zeroV) $
  AwaitInteraction (BuildBuildingsInteraction CanCancelBuilding [Grass, Field]) $
  ActionEnd
workplaceAction DigPassage =
  PerformStep (CollectResourcesStep (stone 1) zeroV) $
  AwaitInteraction (BuildBuildingsInteraction  CanCancelBuilding [Cave, Passage]) $
  ActionEnd
workplaceAction DigCave =
  PerformStep (CollectResourcesStep (stone 1) zeroV) $
  Decision [
    (CaveOrPassageOption ChooseCave,
      AwaitInteraction (BuildBuildingsInteraction  CanCancelBuilding [Cave, Cave]) $
      ActionEnd),
    (CaveOrPassageOption ChoosePassage,
      AwaitInteraction (BuildBuildingsInteraction  CanCancelBuilding [Cave, Passage]) $
      ActionEnd),
    (CaveOrPassageOption NoDigging, ActionEnd)
  ]
workplaceAction WorkerNeed = Decision [
    (WorkerNeedOption HireWorker, PerformStep AddWorkerStep  ActionEnd),
    (WorkerNeedOption BuildRoom, AwaitInteraction (BuildBuildingsInteraction  CannotCancelBuilding [LivingRoom]) ActionEnd)
  ]
workplaceAction ResourceAddition = PerformStep (AddResourcesStep (wood 1 ^+^ stone 1 ^+^ iron 1 ^+^ food 1 ^+^ money 2)) ActionEnd
workplaceAction GatherWood = PerformStep (CollectResourcesStep (wood 1) zeroV) ActionEnd
workplaceAction GatherFood =
  PerformStep (CollectResourcesStep (food 1) zeroV) $
  PerformStep (AddResourcesStep (wheat 1)) $
  AwaitInteraction (BuildBuildingsInteraction  CanCancelBuilding [Grass, Field]) ActionEnd
workplaceAction MakeStartPlayer =
  PerformStep (CollectResourcesStep (food 1) zeroV) $
  PerformStep (AddResourcesStep (iron 2)) $
  PerformStep SetStartPlayerStep  ActionEnd
workplaceAction HouseWork =
  PerformStep AddDog $
  AwaitInteraction (BuildBuildingsInteraction  CanCancelBuilding [LivingRoom]) ActionEnd
workplaceAction Farming =
  AwaitInteraction (BuildBuildingsInteraction CanCancelBuilding [Grass, Field]) $
  AwaitInteraction PlantCropsInteraction  ActionEnd
