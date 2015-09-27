module UniverseTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Universe

universeTests = testGroup "Universe" [
    initialUniverseHasZeroScore,
    initialUniverseHasOneWorker
  ]

initialUniverseHasZeroScore = testCase "Initial universe has zero score" $ do
  let universe = initialUniverse
  0 @=? getScore universe

initialUniverseHasOneWorker = testCase "Initial universe has one worker" $ do
  let workerNumber = length $ getWorkers initialUniverse
  1 @=? workerNumber
