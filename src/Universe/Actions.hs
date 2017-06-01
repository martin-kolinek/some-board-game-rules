module Universe.Actions where

import Control.Lens hiding (universe)
import Data.AdditiveGroup
import Data.List (foldl')
import Data.Map (alter)
import Data.Maybe (fromMaybe)

import Player
import Universe
import Actions
import Universe.Building
import Workplace
import Resources
import Worker
import Building

interactionPrecondition :: ActionInteraction -> WorkerId -> PlayerId -> WorkplaceId -> Universe -> Bool
interactionPrecondition (BuildBuildingsInteraction buildings) _ plId _ universe =
  has (players . ix plId . filtered (playerCanBuildBuildings)) universe
  where playerCanBuildBuildings playerData = all (playerCanBuildBuilding playerData) buildings
interactionPrecondition HireWorkerInteraction _ plId _ universe = has (players . ix plId . filtered playerCanHireWorker) universe
interactionPrecondition ArmWorkerInteraction workerId plId _ universe =
  (has (players . ix plId . playerResources . ironAmount . filtered (> 0)) universe) &&
  (has (players . ix plId . workers . ix workerId . workerStrength . filtered (== 0)) universe)
interactionPrecondition AdventureInteraction workerId plId _ universe =
  (has (players . ix plId . workers . ix workerId . workerStrength . filtered (> 0)) universe)
interactionPrecondition _ _ _ _ _ = True

actionPrecondition :: PlayerId -> WorkerId -> WorkplaceId -> Universe -> ActionDefinition -> Bool
actionPrecondition plId workerId workplaceId universe (CompositeAction composite) = compositeActionPrecondition composite
  where compositeActionPrecondition (InteractionAction interaction _) = interactionPrecondition interaction workerId plId workplaceId universe
        compositeActionPrecondition (OptionalAction _ ) = True
        compositeActionPrecondition (ActionCombination combType interaction1 interaction2) = combinePreconditions combType (compositeActionPrecondition interaction1) (compositeActionPrecondition interaction2)
        combinePreconditions AndThen = const
        combinePreconditions AndOr = (||)
        combinePreconditions Or = (||)
        combinePreconditions AndThenOr = (||)
actionPrecondition _ _ _ _ _ = True

performSteps :: [ActionStep] -> PlayerId -> WorkplaceId -> Universe -> Universe
performSteps acts plId wId universe = foldl' (\u a -> performStep a plId wId u) universe acts

performStep :: ActionStep -> PlayerId -> WorkplaceId -> Universe -> Universe

performStep (AddResourcesStep resources) plId _ universe = universe & (players . ix plId . playerResources %~ (^+^ resources))

performStep CollectResourcesStep plId workplaceId universe =
  let resources = sumV $ universe ^.. (availableWorkplaces . ix workplaceId . workplaceStoredResources)
  in universe &
       players . ix plId . playerResources %~ (^+^ resources) &
       availableWorkplaces . ix workplaceId . workplaceStoredResources .~ zeroV

performStep SetStartPlayerStep plId _ universe = universe & startingPlayer .~ plId

performStep AddDogStep plId _ universe = universe & players . ix plId %~ addDog universe

newDogId :: Universe -> DogId
newDogId universe = DogId (maximum dogNumbers + 1)
  where getNumberFromId (DogId number) = number
        dogNumbers = 0 : toListOf (players . traverse . playerAnimals . dogs . traverse . to getNumberFromId) universe

addDog :: Universe -> PlayerData -> PlayerData
addDog universe = over (buildingSpace . buildingSpaceOccupants) addDogToOccupants . over (playerAnimals . dogs) (dogId :)
  where dogId = newDogId universe
        addDogToOccupants occupants = alter (Just . (DogOccupant dogId :) . fromMaybe []) (0, 0) occupants
