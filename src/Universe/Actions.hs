module Universe.Actions where

import Control.Lens hiding (universe)
import Data.AdditiveGroup

import Player
import Universe
import Actions
import Universe.Building
import Building
import Workplace
import Util
import Universe.Player
import Universe.Worker
import Worker

stepPrecondition :: ActionStep -> PlayerId -> Universe -> Bool
stepPrecondition AddWorkerStep plId = has (players . ix plId . filtered playerCanHireWorker)
stepPrecondition _ _ = const True

interactionPrecondition :: ActionInteraction -> PlayerId -> Universe -> Bool
interactionPrecondition (BuildBuildingsInteraction CannotCancelBuilding buildings) plId universe =
  has (players . ix plId . filtered (playerCanBuildBuildings)) universe
  where playerCanBuildBuildings playerData = all (playerCanBuildBuilding playerData) buildings
interactionPrecondition _ _ _ = True

actionPrecondition :: PlayerId -> Universe -> ActionDefinition -> Bool
actionPrecondition _ _ ActionEnd = True
actionPrecondition plId universe (Decision options) = any (actionPrecondition plId universe) (snd <$> options)
actionPrecondition plId universe (AwaitInteraction interaction continuation) =
  interactionPrecondition interaction plId universe && actionPrecondition plId universe continuation
actionPrecondition plId universe (PerformStep step continuation) =
  stepPrecondition step plId universe && actionPrecondition plId universe continuation

performSteps :: PlayerId -> Universe -> Universe
performSteps plId universe =
  case universe ^? players . ix plId . playerStatus of
    Just (PerformingAction workplaceId (PerformStep step continuation)) ->
      universe &
      advanceStatus plId workplaceId continuation &
      performStep step plId workplaceId &
      performSteps plId
    Just (PerformingAction _ ActionEnd) -> if null $ getOccupantErrors universe plId
      then universe &
           set (players . ix plId . playerStatus) Waiting &
           set (players . ixMaybe (nextPlayer universe plId) . playerStatus) MovingWorker
      else universe
    _ -> universe

advanceStatus :: PlayerId -> WorkplaceId -> ActionDefinition -> Universe -> Universe
advanceStatus plId workplaceId continuation =
      set (players . ix plId . playerStatus) (PerformingAction workplaceId continuation)

performStep :: ActionStep -> PlayerId -> WorkplaceId -> Universe -> Universe

performStep (AddResourcesStep resources) plId _ universe = universe & (players . ix plId . playerResources %~ (^+^ resources))

performStep (CollectResourcesStep _ _) plId workplaceId universe =
  let resources = sumV $ universe ^.. (availableWorkplaces . ix workplaceId . workplaceStoredResources)
  in universe &
       players . ix plId . playerResources %~ (^+^ resources) &
       availableWorkplaces . ix workplaceId . workplaceStoredResources .~ zeroV

performStep AddWorkerStep plId workplaceId universe =
  let addedWorkerId = newWorkerId universe
  in universe &
       players . ix plId . workers . at addedWorkerId .~ Just (WorkerState (Just workplaceId)) &
       players . ix plId . buildingSpace %~ findSpaceForWorker (WorkerOccupant addedWorkerId)

performStep SetStartPlayerStep plId _ universe = universe & startingPlayer .~ plId

performStep AddDog plId _ universe = universe & players . ix plId %~ addDog universe
