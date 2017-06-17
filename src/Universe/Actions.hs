{-# LANGUAGE FlexibleContexts #-}
module Universe.Actions where

import Control.Lens hiding (universe)
import Data.AdditiveGroup
import Data.Map (alter)
import Data.Maybe (fromMaybe)
import Control.Monad.Error.Class
import Control.Monad (foldM)
import Data.Either (isRight)
import Util

import Player
import Universe
import Actions
import Universe.Building
import Workplace
import Resources
import Worker
import Building

interactionPrecondition :: WorkerId -> WorkplaceId -> PlayerId -> Universe -> ActionInteraction -> Bool
interactionPrecondition _ _ plId universe (BuildBuildingsInteraction buildings) =
  has (players . ix plId . filtered (playerCanBuildBuildings)) universe
  where playerCanBuildBuildings playerData = all (playerCanBuildBuilding playerData) buildings
interactionPrecondition _ _ plId universe HireWorkerInteraction = has (players . ix plId . filtered playerCanHireWorker) universe
interactionPrecondition workerId _ plId universe ArmWorkerInteraction =
  (has (players . ix plId . playerResources . ironAmount . filtered (> 0)) universe) &&
  (has (players . ix plId . workers . ix workerId . workerStrength . filtered (== 0)) universe)
interactionPrecondition workerId _ plId universe AdventureInteraction =
  (has (players . ix plId . workers . ix workerId . workerStrength . filtered (> 0)) universe)
interactionPrecondition _ _ _ _ _ = True

stepPrecondition :: WorkerId -> WorkplaceId -> PlayerId -> Universe -> ActionStep -> Bool
stepPrecondition _ workplaceId plId universe step = isRight $ performStep step plId workplaceId universe

actionPrecondition :: PlayerId -> WorkerId -> WorkplaceId -> Universe -> ActionDefinition -> Bool
actionPrecondition plId workerId workplaceId universe (CompositeAction composite) = compositeActionPrecondition composite
  where compositeActionPrecondition (InteractionAction interaction steps) =
          interactionPrecondition workerId workplaceId plId universe interaction &&
          all (stepPrecondition workerId workplaceId plId universe) steps
        compositeActionPrecondition (OptionalAction _) = True
        compositeActionPrecondition (ActionCombination combType interaction1 interaction2) = combinePreconditions combType (compositeActionPrecondition interaction1) (compositeActionPrecondition interaction2)
        combinePreconditions AndThen = const
        combinePreconditions AndOr = (||)
        combinePreconditions Or = (||)
        combinePreconditions AndThenOr = (||)
actionPrecondition _ _ _ _ _ = True

performSteps :: MonadError String m => [ActionStep] -> PlayerId -> WorkplaceId -> Universe -> m Universe
performSteps acts plId wId universe = foldM (\u a -> performStep a plId wId u) universe acts

performStep :: MonadError String m => ActionStep -> PlayerId -> WorkplaceId -> Universe -> m Universe

performStep (AddResourcesStep resources) plId _ universe = return $ universe & (players . ix plId . playerResources %~ (^+^ resources))

performStep CollectResourcesStep plId workplaceId universe = return $
  let resources = sumV $ universe ^.. (availableWorkplaces . ix workplaceId . workplaceStoredResources)
  in universe &
       players . ix plId . playerResources %~ (^+^ resources) &
       availableWorkplaces . ix workplaceId . workplaceStoredResources .~ zeroV

performStep SetStartPlayerStep plId _ universe = return $ universe & startingPlayer .~ plId

performStep AddDogStep plId _ universe = return $ universe & players . ix plId %~ addDog universe

performStep (PayResources resources) plId _ universe = do
  let resourceTrav :: Traversal' Universe Resources
      resourceTrav = players . ix plId . playerResources
      updatedUniverse = universe & resourceTrav %~ (^-^ resources)
  check "Not enough resources" $ has (resourceTrav . filtered isNonNegative) updatedUniverse
  return updatedUniverse

newAnimalId :: Universe -> AnimalId
newAnimalId universe = AnimalId (maximum dogNumbers + 1)
  where getNumberFromId (Animal _ (AnimalId number)) = number
        dogNumbers = 0 : toListOf (players . traverse . playerAnimals . traverse . to getNumberFromId) universe

addDog :: Universe -> PlayerData -> PlayerData
addDog universe = over (buildingSpace . buildingSpaceOccupants) addDogToOccupants . over playerAnimals (dog :)
  where dogId = newAnimalId universe
        dog = (Animal Dog dogId)
        addDogToOccupants occupants = alter (Just . (AnimalOccupant dog :) . fromMaybe []) (0, 0) occupants
