module Universe.Actions where

import Control.Lens hiding (universe)
import Data.AdditiveGroup
import Data.List (foldl')

import Player
import Universe
import Actions
import Universe.Building
import Workplace
import Universe.Player

interactionPrecondition :: ActionInteraction -> PlayerId -> Universe -> Bool
interactionPrecondition (BuildBuildingsInteraction buildings) plId universe =
  has (players . ix plId . filtered (playerCanBuildBuildings)) universe
  where playerCanBuildBuildings playerData = all (playerCanBuildBuilding playerData) buildings
interactionPrecondition HireWorkerInteraction plId universe = has (players . ix plId . filtered playerCanHireWorker) universe
-- interactionPrecondition ArmWorkerInteraction plId universe = liftM2 (&&)
--   (has (players . ix plId . playerResources . ironAmount . filtered (>= strength)))
--   (has (players . ix plId . workers . traverse . inWorkplace workplaceId . workerStrength . filtered (== 0)))
interactionPrecondition _ _ _ = True

actionPrecondition :: PlayerId -> WorkplaceId -> Universe -> ActionDefinition -> Bool
actionPrecondition plId _ universe (CompositeAction composite) = compositeActionPrecondition composite
  where compositeActionPrecondition (InteractionAction interaction _) = interactionPrecondition interaction plId universe
        compositeActionPrecondition (OptionalAction _ ) = True
        compositeActionPrecondition (ActionCombination combType interaction1 interaction2) = combinePreconditions combType (compositeActionPrecondition interaction1) (compositeActionPrecondition interaction2)
        combinePreconditions AndThen = const
        combinePreconditions AndOr = (||)
        combinePreconditions Or = (||)
        combinePreconditions AndThenOr = (||)
actionPrecondition _ _ _ _ = True

performSteps :: [ActionStep] -> PlayerId -> WorkplaceId -> Universe -> Universe
performSteps acts plId wId universe = foldl' (\u a -> performStep a plId wId u) universe acts

performStep :: ActionStep -> PlayerId -> WorkplaceId -> Universe -> Universe

performStep (AddResourcesStep resources) plId _ universe = universe & (players . ix plId . playerResources %~ (^+^ resources))

performStep CollectResourcesStep plId workplaceId universe =
  let resources = sumV $ universe ^.. (availableWorkplaces . ix workplaceId . workplaceStoredResources)
  in universe &
       players . ix plId . playerResources %~ (^+^ resources) &
       availableWorkplaces . ix workplaceId . workplaceStoredResources .~ zeroV

-- performStep (ArmWorkerStep strengthIncrease) plId workplaceId universe = universe &
--   players . ix plId . workers . traverse . inWorkplace workplaceId . workerStrength +~ strengthIncrease

performStep SetStartPlayerStep plId _ universe = universe & startingPlayer .~ plId

performStep AddDogStep plId _ universe = universe & players . ix plId %~ addDog universe
