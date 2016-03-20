{-# LANGUAGE ScopedTypeVariables #-}

module UniverseTest where

import           Data.Either
import           Data.Map         as M
import           Test.Tasty
import           Test.Tasty.HUnit
import           Universe
import Data.List
import Control.Monad
import Data.Maybe

universeTests = testGroup "Universe" [
    initialUniverseHasZeroScore,
    initialUniverseHasTwoPlayers,
    initialUniverseHasOneWorkerForPlayer1,
    initialUniverseHasTwoWorkersForPlayer1,
    initialUniverseHasSixIncreaseScore,
    startWorkingInInvalidWorkplaceCausesError,
    startWorkingByInvalidWorkerCausesError,
    startWorkingByWorkerOfWrongPlayerCausesError,
    workingIncreaseScore,
    workingAssignsWorker,
    finishingTurnNotPossibleWithUnassignedWorkers,
    getWorkplaceOccupantsReturnsOccupants,
    movingSamePlayerTwiceCausesError,
    getCurrentPlayerStartWithFirstPlayer,
    getCurrentPlayerReturnsOtherPlayerAfterMove,
    getCurrentPlayerReturnsNothingWhenAllMovementsWereDone,
    getCurrentPlayerReturnsFirstPlayerAfterFullTurn,
    firstPlayerCanMoveAfterFinishTurn
  ]

initialUniverseHasTwoPlayers = testCase "Initial universe has two players" $
  2 @=? length (getPlayers initialUniverse)

initialUniverseHasZeroScore = testCase "Initial universe has zero score" $ do
  let universe = initialUniverse
  all (== 0) (getScore universe <$> getPlayers universe) @? "Score not zero"

player1 = head (getPlayers initialUniverse)

player2 = getPlayers initialUniverse !! 1

initialUniverseHasOneWorkerForPlayer1 = testCase "Initial universe has one worker for player 1" $ do
  let workerNumber = length $ getWorkers initialUniverse player1
  2 @=? workerNumber

initialUniverseHasTwoWorkersForPlayer1 = testCase "Initial universe has one worker for player 2" $ do
  let workerNumber = length $ getWorkers initialUniverse player2
  3 @=? workerNumber

initialUniverseHasSixIncreaseScore = testCase "Initial universe has six increase score workplaces" $
  replicate 6 IncreaseScore @=? elems (getWorkplaces initialUniverse)

firstWorkplace = (head . keys) (getWorkplaces initialUniverse)

startWorkingInInvalidWorkplaceCausesError = testCase "Start working in invalid workplace causes error" $ do
  let universe = initialUniverse
      worker = head $ getWorkers universe player1
      nextUniverse = startWorking worker (WorkplaceId 50) universe
  assertBool "No error" $ isLeft nextUniverse

startWorkingByInvalidWorkerCausesError = testCase "Start working in invalid workplace causes error" $ do
  let universe = initialUniverse
      nextUniverse = startWorking (WorkerId 50) firstWorkplace universe
  assertBool "No error" $ isLeft nextUniverse

startWorkingByWorkerOfWrongPlayerCausesError = testCase "Start working by worker of wrong player causes error" $ do
  let nextUniverse = startWorking (head $ getWorkers initialUniverse player2) firstWorkplace initialUniverse
  assertBool "No error" $ isLeft nextUniverse

increaseScoreWorkplace = (head . keys) (M.filter (==IncreaseScore) (getWorkplaces initialUniverse))

initialWorker = head $ getWorkers initialUniverse player1

workingIncreaseScore = testCase "Start working in increate score, then score is incresed" $ do
  let workplace = increaseScoreWorkplace
      worker = initialWorker
      Right nextUniverse = startWorking worker workplace initialUniverse
  1 @=? getScore nextUniverse (head $ getPlayers initialUniverse)

workingAssignsWorker = testCase "Start working assigns worker" $ do
  let workplace2 = increaseScoreWorkplace
      worker = initialWorker
      Right nextUniverse = startWorking worker workplace2 initialUniverse
  Just workplace2 @=? getWorkerWorkplace nextUniverse worker

afterAllMoves =
  let workers = (concat . transpose) [getWorkers initialUniverse player1, getWorkers initialUniverse player2]
      workplaces = keys $ getWorkplaces initialUniverse
      workersWithWorkplaces = zip workers workplaces
      Right afterWorking = foldM (flip $ uncurry startWorking) initialUniverse workersWithWorkplaces
  in afterWorking

finishingTurnUnassignsWorkers = testCase "Finishing turn unassigns works" $ do
  let Right afterFinishingTurn = finishTurn afterAllMoves
  assertBool "Occupied workplace" $ all (Data.List.null . getWorkplaceOccupants afterFinishingTurn) (keys $ getWorkplaces afterFinishingTurn)
  let allWorkersFree playerId = all (isNothing . getWorkerWorkplace afterFinishingTurn) (getWorkers afterFinishingTurn playerId)
  assertBool "Worker on a workplace" $ all allWorkersFree (getPlayers afterFinishingTurn)

finishingTurnNotPossibleWithUnassignedWorkers = testCase "Finishing turn unassigns works" $ do
  let universe = initialUniverse
  assertBool "No error" $ isLeft (finishTurn universe)

movingSamePlayerTwiceCausesError = testCase "Moving the same player twice causes error" $ do
  let workers = getWorkers initialUniverse player1
      worker1 = head workers
      worker2 = workers !! 1
      workplaces = keys $ getWorkplaces initialUniverse
      workplace1 = head workplaces
      workplace2 = workplaces !! 1
      Right startedWorking = startWorking worker1 workplace1 initialUniverse
  assertBool "No error" $ isLeft (startWorking worker2 workplace2 startedWorking)

getWorkplaceOccupantsReturnsOccupants = testCase "getWorkplaceOccupands returns occupants" $ do
  let workers = getWorkers initialUniverse player1
      worker1 = head workers
      workplaces = keys $ getWorkplaces initialUniverse
      workplace1 = head workplaces
      Right startedWorking = startWorking worker1 workplace1 initialUniverse
  [worker1] @=? getWorkplaceOccupants startedWorking workplace1

getCurrentPlayerStartWithFirstPlayer = testCase "getCurrentPlayer starts with first player" $
  Just player1 @=? getCurrentPlayer initialUniverse

getCurrentPlayerReturnsOtherPlayerAfterMove = testCase "getCurrentPlayer returns other player after move" $ do
  let Right afterMove = startWorking (head $ getWorkers initialUniverse player1) ((head . keys . getWorkplaces) initialUniverse) initialUniverse
  Just player2 @=? getCurrentPlayer afterMove

getCurrentPlayerReturnsNothingWhenAllMovementsWereDone = testCase "getCurrentPlayer returns nothing when all movements were done" $
  Nothing @=? getCurrentPlayer afterAllMoves

getCurrentPlayerReturnsFirstPlayerAfterFullTurn = testCase "getCurrentPlayer returns first player after complete turn" $ do
  let Right afterFullTurn = finishTurn afterAllMoves
  Just player1 @=? getCurrentPlayer afterFullTurn

firstPlayerCanMoveAfterFinishTurn = testCase "First player can move after finish turn" $ do
  let Right afterFullTurn = finishTurn afterAllMoves
  assertBool "Cannot move" $ isRight $ startWorking (head $ getWorkers afterFullTurn player1) (head $ keys $ getWorkplaces afterFullTurn) afterFullTurn
