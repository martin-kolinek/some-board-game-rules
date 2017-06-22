module Workplaces.WorkerNeed where

import Rules
import TestFramework
import TestHelpers

import Data.List (sort, (\\), nub)
import Data.Map (elems)
import qualified Data.Set as S
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

workerNeedTests :: TestTree
workerNeedTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Worker need tests" $ [
  testProperty "Cannot start working unless there is enough space for worker or living room" $ movingWorkerProperty $ do
      universe <- getUniverse
      pre $ not $ currentPlayerCanBuildRoom universe || currentPlayerHasFreeRoom universe
      _ <- startWorkingInWorkerNeed
      shouldHaveFailed,
    testProperty "Cannot choose hire worker unless there is space" $ movingWorkerProperty $ do
      (plId, _, _) <- checkedStartWorkingInWorkerNeed
      universe <- getUniverse
      pre $ not $ currentPlayerHasFreeRoom universe
      applyToUniverse $ hireWorker plId
      shouldHaveFailed,
    testProperty "Choosing hire worker starts next player" $ movingWorkerProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkPlayerHasValidOccupants playerId
      checkedHireWorker
      validateNextPlayer playerId,
    testProperty "Choosing hire worker keeps occupants valid" $ movingWorkerProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkPlayerHasValidOccupants playerId
      checkedHireWorker
      validatePlayerHasValidOccupants playerId,
    testProperty "Choosing hire worker places worker" $ movingWorkerProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkPlayerHasValidOccupants playerId
      checkedHireWorker
      resultUniverse <- getUniverse
      let newWorkerOccupants = S.fromList $ WorkerOccupant <$> getWorkers resultUniverse playerId \\ getWorkers originalUniverse playerId
          occupants = S.fromList $ concat $ elems $ getBuildingOccupants resultUniverse playerId
      assert $ S.null $ S.difference newWorkerOccupants occupants,
    testProperty "Choosing hire worker adds worker" $ movingWorkerProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkPlayerHasValidOccupants playerId
      checkedHireWorker
      resultUniverse <- getUniverse
      assert $ length (getWorkers resultUniverse playerId \\ getWorkers originalUniverse playerId) == 1,
    testProperty "New worker is unique" $ movingWorkerProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkPlayerHasValidOccupants playerId
      checkedHireWorker
      universe <- getUniverse
      let workerIds = sort [workerId | plId <- getPlayers universe, workerId <- getWorkers universe plId]
      assert $ workerIds == nub workerIds,
    testProperty "Selecting invalid position is not possible while building room" $ movingWorkerProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkPlayerHasValidOccupants playerId
      checkRoomPrecondition
      _ <- selectWrongPosition availableSingleCavePositions playerId
      shouldHaveFailed,
    testProperty "Selecting valid position builds room while building room" $ movingWorkerProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkPlayerHasValidOccupants playerId
      checkRoomPrecondition
      (pos, _) <- selectCorrectPosition availableSingleCavePositions playerId
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ SmallBuilding LivingRoom pos `elem` buildings,
    testProperty "Building room subtracts resources" $ movingWorkerProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkPlayerHasValidOccupants playerId
      checkRoomPrecondition
      origResources <- getsUniverse getPlayerResources <*> pure playerId
      (_, _) <- selectCorrectPosition availableSingleCavePositions playerId
      newResources <- getsUniverse getPlayerResources <*> pure playerId
      assert (getWoodAmount newResources == getWoodAmount origResources - 4)
      assert (getStoneAmount newResources == getStoneAmount origResources - 3),
    testProperty "Selecting valid position ends turn while building room" $ movingWorkerProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkPlayerHasValidOccupants playerId
      checkRoomPrecondition
      _ <- selectCorrectPosition availableSingleCavePositions playerId
      validateNextPlayer playerId
  ]

startWorkingInWorkerNeed :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInWorkerNeed = startWorkingInWorkplaceType WorkerNeed

checkedStartWorkingInWorkerNeed :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
checkedStartWorkingInWorkerNeed = do
  universe <- getUniverse
  pre $ currentPlayerHasFreeRoom universe || currentPlayerCanBuildRoom universe
  startWorkingInWorkerNeed

checkCurrentPlayerHasFreeRoom :: UniversePropertyMonad ()
checkCurrentPlayerHasFreeRoom = do
  universe <- getUniverse
  pre $ currentPlayerHasFreeRoom universe

checkedHireWorker :: UniversePropertyMonad ()
checkedHireWorker = do
  checkCurrentPlayerHasFreeRoom
  Just plId <- getsUniverse getCurrentPlayer
  applyToUniverse $ hireWorker plId

checkRoomPrecondition :: UniversePropertyMonad ()
checkRoomPrecondition = do
  u <- getUniverse
  pre $ currentPlayerCanBuildRoom u
