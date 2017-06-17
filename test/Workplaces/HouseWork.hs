module Workplaces.HouseWork where

import Rules
import TestHelpers
import TestFramework

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.Set as S
import Data.Map (elems)
import Control.Monad (join)

houseWorkTests :: TestTree
houseWorkTests = localOption (QuickCheckMaxRatio 500) $ testGroup "House work tests" $ [
    testProperty "After working, player is building living room and can cancel" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInHouseWork
      buildings <- getsUniverse currentlyBuiltBuildings <*> pure playerId
      assert $ buildings == [[LivingRoom]],
    testProperty "After working and collecting resources, a dog is added" $ movingWorkerProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, _) <- startWorkingInHouseWork
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ collectResources playerId
      newUniverse <- getUniverse
      let originalDog = S.fromList $ getAnimals originalUniverse playerId
          newDog = S.fromList $ getAnimals newUniverse playerId
          animalDifference = (newDog S.\\ originalDog)
          isDog (Animal Dog _) = True
          isDog _ = False
      assert $ S.size animalDifference == 1
      assert $ all isDog animalDifference,
    testProperty "After working and collecting resources a dog is added to occupants" $ movingWorkerProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, _) <- startWorkingInHouseWork
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ collectResources playerId
      newUniverse <- getUniverse
      let isDogOccupant (AnimalOccupant (Animal Dog _)) = True
          isDogOccupant _ = False
          getDogOccupants universe = (S.fromList . filter isDogOccupant . join . elems) (getBuildingOccupants universe playerId)
          originalDogOccupants = getDogOccupants originalUniverse
          newDogOccupants = getDogOccupants newUniverse
      assert $ S.size (newDogOccupants S.\\ originalDogOccupants) == 1,
    testProperty "Working keeps occupants valid" $ movingWorkerProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, _) <- startWorkingInHouseWork
      pre $ getOccupantErrors originalUniverse playerId == []
      validatePlayerHasValidOccupants playerId,
    testProperty "After working collecting resources and finishing action, next player moves" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInHouseWork
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ collectResources playerId
      applyToUniverse $ finishAction playerId
      validateNextPlayer playerId,
    testProperty "Selecting invalid position is not possible" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInHouseWork
      checkPlayerHasValidOccupants playerId
      _ <- selectWrongPosition availableSingleCavePositions playerId
      shouldHaveFailed,
    testProperty "Selecting valid position builds a living room" $ movingWorkerProperty $ do
      (playerId, _, _) <- startWorkingInHouseWork
      checkPlayerHasValidOccupants playerId
      pre =<< getsUniverse currentPlayerCanBuildRoom
      (pos, _) <- selectCorrectPosition availableSingleCavePositions playerId
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ Building LivingRoom pos `elem` buildings
  ]

startWorkingInHouseWork :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInHouseWork = startWorkingInWorkplaceType HouseWork
