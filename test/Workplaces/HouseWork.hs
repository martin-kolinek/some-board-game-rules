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
    testProperty "After working decisions are available" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInHouseWork
      decisions <- getsUniverse getPossibleDecisions <*> pure playerId
      assert $ (S.fromList decisions) == S.fromList [AnyRoomOption ChooseNoRoom, AnyRoomOption ChooseLivingRoom],
    testProperty "After working a dog is added" $ universeProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, _) <- startWorkingInHouseWork
      newUniverse <- getUniverse
      let originalDog = S.fromList $ getDogs originalUniverse playerId
          newDog = S.fromList $ getDogs newUniverse playerId
      assert $ S.size (newDog S.\\ originalDog) == 1,
    testProperty "After working a dog is added" $ universeProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, _) <- startWorkingInHouseWork
      newUniverse <- getUniverse
      let isDogOccupant (DogOccupant _) = True
          isDogOccupant _ = False
          getDogOccupants universe = (S.fromList . filter isDogOccupant . join . elems) (getBuildingOccupants universe playerId)
          originalDogOccupants = getDogOccupants originalUniverse
          newDogOccupants = getDogOccupants newUniverse
      assert $ S.size (newDogOccupants S.\\ originalDogOccupants) == 1,
    testProperty "Working keeps occupants valid" $ universeProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, _) <- startWorkingInHouseWork
      pre $ getOccupantErrors originalUniverse playerId == []
      validatePlayerHasValidOccupants playerId,
    testProperty "After working and selecting no room, next player moves" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInHouseWork
      checkPlayerHasValidOccupants playerId
      applyToUniverse $ chooseOption playerId (AnyRoomOption ChooseNoRoom)
      validateNextPlayer playerId,
    testProperty "Selecting invalid position is not possible" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInHouseWork
      applyToUniverse $ chooseOption playerId (AnyRoomOption ChooseLivingRoom)
      _ <- selectWrongPosition availableSingleCavePositions playerId
      shouldHaveFailed,
    testProperty "Selecting valid position builds a living room" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInHouseWork
      applyToUniverse $ chooseOption playerId (AnyRoomOption ChooseLivingRoom)
      (pos, _) <- selectCorrectPosition availableSingleCavePositions playerId
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ Building LivingRoom pos `elem` buildings
  ]

startWorkingInHouseWork :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInHouseWork = startWorkingInWorkplaceType HouseWork
