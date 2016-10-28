module Workplaces.WorkerNeed where

import Rules

import Data.List (sort, (\\), nub)
import Data.Map (elems)
import qualified Data.Set as S

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import TestFramework
import RulesProperties

workerNeedTests :: TestTree
workerNeedTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Worker need tests" $ [
    testProperty "Cannot start working unless there is enough space for worker or living room" $ universeProperty $ do
      universe <- getUniverse
      pre $ not $ currentPlayerCanBuildRoom universe || currentPlayerHasFreeRoom universe
      _ <- startWorkingInWorkerNeed
      shouldHaveFailed,
    testProperty "Can make decisions after starting working" $ universeProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      decisions <- getsUniverse getPossibleDecisions <*> pure playerId
      assert $ sort decisions == sort [WorkerNeedOption BuildRoom, WorkerNeedOption HireWorker],
    testProperty "Cannot choose hire worker unless there is space" $ universeProperty $ do
      _ <- checkedStartWorkingInWorkerNeed
      universe <- getUniverse
      pre $ not $ currentPlayerHasFreeRoom universe
      applyToUniverse $ chooseOption $ WorkerNeedOption HireWorker
      shouldHaveFailed,
    testProperty "Choosing hire worker starts next player" $ universeProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkPlayerHasValidOccupants playerId
      checkedHireWorker
      validateNextPlayer playerId,
    testProperty "Choosing hire worker keeps occupants valid" $ universeProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkPlayerHasValidOccupants playerId
      checkedHireWorker
      validatePlayerHasValidOccupants playerId,
    testProperty "Choosing hire worker places worker" $ universeProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkedHireWorker
      resultUniverse <- getUniverse
      let newWorkerOccupants = S.fromList $ WorkerOccupant <$> getWorkers resultUniverse playerId \\ getWorkers originalUniverse playerId
          occupants = S.fromList $ concat $ elems $ getBuildingOccupants resultUniverse playerId
      assert $ S.null $ S.difference newWorkerOccupants occupants,
    testProperty "Choosing hire worker adds worker" $ universeProperty $ do
      originalUniverse <- getUniverse
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkedHireWorker
      resultUniverse <- getUniverse
      assert $ length (getWorkers resultUniverse playerId \\ getWorkers originalUniverse playerId) == 1,
    testProperty "New worker is unique" $ universeProperty $ do
      _ <- checkedStartWorkingInWorkerNeed
      checkedHireWorker
      universe <- getUniverse
      let workerIds = sort [workerId | plId <- getPlayers universe, workerId <- getWorkers universe plId]
      assert $ workerIds == nub workerIds,
    testProperty "Cannot build living room without space" $ universeProperty $ do
      _ <- checkedStartWorkingInWorkerNeed
      universe <- getUniverse
      pre $ not $ currentPlayerCanBuildRoom universe
      applyToUniverse $ chooseOption $ WorkerNeedOption BuildRoom
      shouldHaveFailed,
    testProperty "Canceling selection is not possible while building room" $ universeProperty $ do
      _ <- checkedStartWorkingInWorkerNeed
      checkedBuildRoom
      applyToUniverse cancelSelection
      shouldHaveFailed,
    testProperty "Selecting invalid position is not possible while building room" $ universeProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkedBuildRoom
      _ <- selectWrongPosition availableSingleCavePositions playerId
      shouldHaveFailed,
    testProperty "Selecting valid position builds room while building room" $ universeProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkedBuildRoom
      (pos, _) <- selectCorrectPosition availableSingleCavePositions playerId
      buildings <- getsUniverse getBuildingSpace <*> pure playerId
      assert $ LivingRoom pos `elem` buildings,
    testProperty "Selecting valid position ends turn while building room" $ universeProperty $ do
      (playerId, _, _) <- checkedStartWorkingInWorkerNeed
      checkPlayerHasValidOccupants playerId
      checkedBuildRoom
      _ <- selectCorrectPosition availableSingleCavePositions playerId
      validateNextPlayer playerId
  ]

startWorkingInWorkerNeed :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInWorkerNeed = startWorkingInWorkplaceType (== WorkerNeed)

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
  applyToUniverse $ chooseOption $ WorkerNeedOption HireWorker

checkedBuildRoom :: UniversePropertyMonad ()
checkedBuildRoom = do
  universe <- getUniverse
  pre $ currentPlayerCanBuildRoom universe
  applyToUniverse $ chooseOption $ WorkerNeedOption BuildRoom
