module Universe.Actions where

import Control.Lens hiding (universe)
import Data.AdditiveGroup
import Control.Monad (liftM2)

import Player
import Universe
import Actions
import Universe.Building
import Building
import Workplace
import Util
import Resources
import Universe.Player
import Universe.Worker
import Worker

stepPrecondition :: ActionStep -> PlayerId -> WorkplaceId -> Universe -> Bool
stepPrecondition AddWorkerStep plId _ = has (players . ix plId . filtered playerCanHireWorker)
stepPrecondition (ArmWorkerStep strength) plId workplaceId = liftM2 (&&)
  (has (players . ix plId . playerResources . ironAmount . filtered (>= strength)))
  (has (players . ix plId . workers . traverse . inWorkplace workplaceId . workerStrength . filtered (== 0)))
stepPrecondition _ _ _ = const True

interactionPrecondition :: ActionInteraction -> PlayerId -> Universe -> Bool
interactionPrecondition (BuildBuildingsInteraction CannotCancelBuilding buildings) plId universe =
  has (players . ix plId . filtered (playerCanBuildBuildings)) universe
  where playerCanBuildBuildings playerData = all (playerCanBuildBuilding playerData) buildings
interactionPrecondition _ _ _ = True

actionPrecondition :: PlayerId -> WorkplaceId -> Universe -> ActionDefinition -> Bool
actionPrecondition _ _ _ ActionEnd = True
actionPrecondition plId workplaceId universe (Decision options) = any (actionPrecondition plId workplaceId universe) (snd <$> options)
actionPrecondition plId workplaceId universe (AwaitInteraction interaction continuation) =
  interactionPrecondition interaction plId universe && actionPrecondition plId workplaceId universe continuation
actionPrecondition plId workplaceId universe (PerformStep step continuation) =
  stepPrecondition step plId workplaceId universe && actionPrecondition plId workplaceId universe continuation

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
       players . ix plId . workers . at addedWorkerId . non initialWorkerState . currentWorkplace .~ Just workplaceId &
       players . ix plId . buildingSpace %~ findSpaceForWorker (WorkerOccupant addedWorkerId)

performStep (ArmWorkerStep strengthIncrease) plId workplaceId universe = universe &
  players . ix plId . workers . traverse . inWorkplace workplaceId . workerStrength +~ strengthIncrease

performStep SetStartPlayerStep plId _ universe = universe & startingPlayer .~ plId

performStep AddDogStep plId _ universe = universe & players . ix plId %~ addDog universe
