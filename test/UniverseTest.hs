{-# LANGUAGE ScopedTypeVariables #-}

module UniverseTest where

import           Data.Either
import           Data.Map         as M
import           Test.Tasty
import           Test.Tasty.HUnit
import           Universe

universeTests = testGroup "Universe" [
    initialUniverseHasZeroScore,
    initialUniverseHasOneWorker,
    initialUniverseHasOneIncreaseScore,
    startWorkingInInvalidWorkplaceCausesError,
    startWorkingByInvalidWorkerCausesError,
    workingIncreaseScore,
    workingAssignsWorker,
    finishingTurnNotPossibleWithUnassignedWorkers
  ]

initialUniverseHasZeroScore = testCase "Initial universe has zero score" $ do
  let universe = initialUniverse
  0 @=? getScore universe

initialUniverseHasOneWorker = testCase "Initial universe has one worker" $ do
  let workerNumber = length $ getWorkers initialUniverse
  1 @=? workerNumber

initialUniverseHasOneIncreaseScore = testCase "Initial universe has increase score" $
  [IncreaseScore] @=? elems (getWorkplaces initialUniverse)

startWorkingInInvalidWorkplaceCausesError = testCase "Start working in invalid workplace causes error" $ do
  let universe = initialUniverse
  let worker = head $ getWorkers universe
  let nextUniverse :: Either String Universe = startWorking worker (WorkplaceId 50) universe
  assertBool "No error" $ isLeft nextUniverse

startWorkingByInvalidWorkerCausesError = testCase "Start working in invalid workplace causes error" $ do
  let universe = initialUniverse
  let workplace = (head . keys) (getWorkplaces universe)
  let nextUniverse :: Either String Universe = startWorking (WorkerId 50) workplace universe
  assertBool "No error" $ isLeft nextUniverse

increaseScoreWorkplace = (head . keys) (M.filter (==IncreaseScore) (getWorkplaces initialUniverse))

initialWorker = head $ getWorkers initialUniverse

workingIncreaseScore = testCase "Start working in increate score, then score is incresed" $ do
  let workplace = increaseScoreWorkplace
  let worker = initialWorker
  let Right nextUniverse = startWorking worker workplace initialUniverse
  1 @=? getScore nextUniverse

workingAssignsWorker = testCase "Start working assigns worker" $ do
  let workplace2 = increaseScoreWorkplace
  let worker = initialWorker
  let Right nextUniverse = startWorking worker workplace2 initialUniverse
  Just workplace2 @=? getWorkerWorkplace nextUniverse worker

finishingTurnUnassignsWorkers = testCase "Finishing turn unassigns works" $ do
  let universe = Universe (fromList [(WorkplaceId 1, IncreaseScore)]) (fromList [(WorkerId 1, WorkerState (Just (WorkplaceId 1)))]) 0
  let Right nextUniverse = finishTurn universe
  Nothing @=? getWorkerWorkplace nextUniverse (WorkerId 1)

finishingTurnNotPossibleWithUnassignedWorkers = testCase "Finishing turn unassigns works" $ do
  let universe = initialUniverse
  assertBool "No error" $ isLeft (finishTurn universe)
