{-# LANGUAGE ScopedTypeVariables #-}

module UniverseTest where

import           Data.Either
import           Data.Map         as M
import           Test.Tasty
import           Test.Tasty.HUnit
import           Universe
import Debug.Trace

universeTests = testGroup "Universe" [
    initialUniverseHasZeroScore,
    initialUniverseHasOneWorker,
    initialUniverseHasOneIncreaseScore,
    startWorkingInInvalidWorkplaceCausesError,
    startWorkingByInvalidWorkerCausesError,
    workingIncreaseScore,
    workingAssignsWorker,
    finishingTurnNotPossibleWithUnassignedWorkers,
    getWorkplaceOccupantsReturnsOccupants
  ]

initialUniverseHasZeroScore = testCase "Initial universe has zero score" $ do
  let universe = initialUniverse
  all (== 0) (getScore universe <$> getPlayers universe) @? "Score not zero"

initialUniverseHasOneWorker = testCase "Initial universe has one worker" $ do
  let workerNumber = length $ getWorkers initialUniverse (head $ getPlayers initialUniverse)
  1 @=? workerNumber

initialUniverseHasOneIncreaseScore = testCase "Initial universe has increase score" $
  replicate 6 IncreaseScore @=? elems (getWorkplaces initialUniverse)

startWorkingInInvalidWorkplaceCausesError = testCase "Start working in invalid workplace causes error" $ do
  let universe = initialUniverse
  let worker = head $ getWorkers universe (head $ getPlayers universe)
  let nextUniverse :: Either String Universe = startWorking worker (WorkplaceId 50) universe
  assertBool "No error" $ isLeft nextUniverse

startWorkingByInvalidWorkerCausesError = testCase "Start working in invalid workplace causes error" $ do
  let universe = initialUniverse
  let workplace = (head . keys) (getWorkplaces universe)
  let nextUniverse :: Either String Universe = startWorking (WorkerId 50) workplace universe
  assertBool "No error" $ isLeft nextUniverse

increaseScoreWorkplace = (head . keys) (M.filter (==IncreaseScore) (getWorkplaces initialUniverse))

initialWorker = head $ getWorkers initialUniverse (head $ getPlayers initialUniverse)

workingIncreaseScore = testCase "Start working in increate score, then score is incresed" $ do
  let workplace = increaseScoreWorkplace
  let worker = initialWorker
  let Right nextUniverse = traceShowId (startWorking worker workplace initialUniverse)
  1 @=? getScore nextUniverse (head $ getPlayers initialUniverse)

workingAssignsWorker = testCase "Start working assigns worker" $ do
  let workplace2 = increaseScoreWorkplace
  let worker = initialWorker
  let Right nextUniverse = startWorking worker workplace2 initialUniverse
  Just workplace2 @=? getWorkerWorkplace nextUniverse worker

finishingTurnUnassignsWorkers = testCase "Finishing turn unassigns works" $ return ()


finishingTurnNotPossibleWithUnassignedWorkers = testCase "Finishing turn unassigns works" $ do
  let universe = initialUniverse
  assertBool "No error" $ isLeft (finishTurn universe)

getWorkplaceOccupantsReturnsOccupants = testCase "getWorkplaceOccupands returns occupants" $ return ()
