{-# LANGUAGE ScopedTypeVariables, ImpredicativeTypes #-}

module UniverseTest where

import Data.Either
import Data.Map         as M
import Test.Tasty
import Test.Tasty.HUnit as H
import Universe
import Data.List
import Control.Monad
import Data.Maybe
import TestFramework
import Control.Monad.IO.Class
import Control.Monad.State

universeTests = testGroup "Universe" [
    universeTestCase "Initial universe has two players" $ do
      universe <- readUniverse
      liftIO $ 2 @=? (length . getPlayers) universe
    ,
    universeTestCase "Initial universe has zero score" $ do
      universe <- readUniverse
      liftIO $ forM_ (getPlayers universe) $ \player -> 0 @=? getScore universe player
    ,
    universeTestCase "Initial universe has two workers for player 1" $ do
      workers <- getWorkers <$> readUniverse <*> player1
      liftIO $ 2 @=? length workers
    ,
    universeTestCase "Initial universe has three worker for player 2" $ do
      workers <- getWorkers <$> readUniverse <*> player2
      liftIO $ 3 @=? length workers
    ,
    universeTestCase "Initial universe has six increase score workplaces" $ do
      workplaces <- getWorkplaces <$> readUniverse
      liftIO $ replicate 6 IncreaseScore @=? elems workplaces
    ,
    universeTestCaseFailure "Start working in invalid workplace causes error" $ do
      worker <- getWorker 0 0
      startWorking worker (WorkplaceId 50) =<< readUniverse
      return ()
    ,
    universeTestCaseFailure "Start working by invalid worker causes error" $ do
      workplace <- getWorkplace 0
      startWorking (WorkerId 50) workplace =<< readUniverse
      return ()
    ,
    universeTestCase "Start working in increate score, then score is incresed" $ do
      startWorkingFirstWorker
      player1Score <- getScore <$> readUniverse <*> player1
      liftIO $ 1 @=? player1Score
    ,
    universeTestCase "Start working assigns worker" $ do
      startWorkingFirstWorker
      workerWorkplace <- getWorkerWorkplace <$> readUniverse <*> getWorker 0 0
      firstWorkplace <- getWorkplace 0
      liftIO $ Just firstWorkplace @=? workerWorkplace
    ,
    universeTestCase "Finishing turn unassigns works" $ do
      makeAllMovesInTurn1
      applyToUniverse finishTurn
      workplaceOccupantsFunc <- gets getWorkplaceOccupants
      workplaces <- gets $ keys . getWorkplaces
      liftIO $ assertBool "Occupied workplace" $ all (Data.List.null . workplaceOccupantsFunc) workplaces
      workerWorkplaceFunc <- gets getWorkerWorkplace
      workersFunc <- gets getWorkers
      let allWorkersFree playerId = all (isNothing . workerWorkplaceFunc) (workersFunc playerId)
      players <- gets getPlayers
      liftIO $ assertBool "Worker on a workplace" $ all allWorkersFree players
    ,
    universeTestCaseFailure "Finishing turn is not possible without assigned workers" $
      applyToUniverse finishTurn
    ,
    universeTestCaseFailure "Moving the same player twice causes error" $ do
      worker1 <- getWorker 0 0
      workplace1 <- getWorkplace 0
      workplace2 <- getWorkplace 2
      applyToUniverse $ startWorking worker1 workplace1
      applyToUniverse $ startWorking worker1 workplace2
    ,
    universeTestCase "Workplace occupants can be found" $ do
      startWorkingFirstWorker
      workplaceOccupants <- gets getWorkplaceOccupants <*> getWorkplace 0
      worker <- getWorker 0 0
      liftIO $ [worker] @=? workplaceOccupants
    ,
    universeTestCase "Current player is the first player in the beginning" $ do
      player <- getPlayer 0
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Just player @=? currentPlayer
    ,
    universeTestCase "Current player is second player after first move" $ do
      startWorkingFirstWorker
      player <- getPlayer 1
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Just player @=? currentPlayer
    ,
    universeTestCase "There is no current player after all moves in turn" $ do
      makeAllMovesInTurn1
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Nothing @=? currentPlayer
    ,
    universeTestCase "Current player is first player after finishing first turn" $ do
      makeAllMovesInTurn1
      applyToUniverse finishTurn
      player <- getPlayer 0
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Just player @=? currentPlayer
    ,
    universeTestCase "First player can move after finishing first turn" $ do
      makeAllMovesInTurn1
      applyToUniverse finishTurn
      startWorkingFirstWorker
  ]

startWorkingFirstWorker = do
  workplace <- getWorkplace 0
  worker <- getWorker 0 0
  applyToUniverse $ startWorking worker workplace

makeAllMovesInTurn1 :: UniverseTest ()
makeAllMovesInTurn1 = do
  universe <- readUniverse
  let players = getPlayers universe
      workers = (concat . transpose) (getWorkers universe <$> players)
      workplaces = keys $ getWorkplaces universe
      startWorkingCurried x = applyToUniverse (uncurry startWorking x)
  forM_ (zip workers workplaces) startWorkingCurried
