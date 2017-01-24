module Universe.Actions where

import Control.Lens hiding (universe)

import Player
import Universe
import Actions
import Universe.Building
import Building

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
