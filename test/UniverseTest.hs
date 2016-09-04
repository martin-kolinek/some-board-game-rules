{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, ImpredicativeTypes #-}

module UniverseTest where

import Data.Either
import Data.Map         as M
import Test.Tasty
import Test.Tasty.HUnit as H
import Data.List
import Control.Monad
import Data.Maybe
import TestFramework
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Default
import Control.Lens

import Workplace
import Worker
import Building
import Player
import Resources
import Universe
import Universe.Player
import Universe.Worker
import Universe.Workplace
import Universe.Actions
import Universe.Building

instance Default Universe where
  def = initialUniverse

universeTests = testGroup "Universe" [
    flowTestCase "Initial universe has two players" $ do
      universe <- get
      liftIO $ 2 @=? (length . getPlayers) universe
    ,
    flowTestCase "Initial universe has two workers for player 1" $ do
      workers <- getWorkers <$> get <*> player1
      liftIO $ 2 @=? length workers
    ,
    flowTestCase "Initial universe has three worker for player 2" $ do
      workers <- getWorkers <$> get <*> player2
      liftIO $ 3 @=? length workers
    ,
    flowTestCase "Initial universe has six increase score workplaces" $ do
      workplaces <- getWorkplaces <$> get
      liftIO $ replicate 12 CutForest @=? elems workplaces
    ,
    flowTestCaseFailure "Start working in invalid workplace causes error" $ do
      worker <- getWorker 0 0
      startWorking worker (WorkplaceId 50) =<< get
      return ()
    ,
    flowTestCaseFailure "Start working by invalid worker causes error" $ do
      workplace <- getWorkplace 0
      startWorking (WorkerId 50) workplace =<< get
      return ()
    ,
    flowTestCase "Start working assigns worker" $ do
      startWorkingFirstWorker
      workerWorkplace <- getWorkerWorkplace <$> get <*> getWorker 0 0
      firstWorkplace <- getWorkplace 0
      liftIO $ Just firstWorkplace @=? workerWorkplace
    ,
    flowTestCase "Finishing turn unassigns workers" $ do
      makeAllMovesInTurn1
      apply finishTurn
      workplaceOccupantsFunc <- gets getWorkplaceOccupants
      workplaces <- gets $ keys . getWorkplaces
      liftIO $ assertBool "Occupied workplace" $ all (Data.List.null . workplaceOccupantsFunc) workplaces
      workerWorkplaceFunc <- gets getWorkerWorkplace
      workersFunc <- gets getWorkers
      let allWorkersFree playerId = all (isNothing . workerWorkplaceFunc) (workersFunc playerId)
      players <- gets getPlayers
      liftIO $ assertBool "Worker on a workplace" $ all allWorkersFree players
    ,
    flowTestCaseFailure "Finishing turn is not possible without assigned workers" $
      apply finishTurn
    ,
    flowTestCaseFailure "Moving the same player twice causes error" $ do
      worker1 <- getWorker 0 0
      workplace1 <- getWorkplace 0
      workplace2 <- getWorkplace 2
      apply $ startWorking worker1 workplace1
      apply $ startWorking worker1 workplace2
    ,
    flowTestCase "Workplace occupants can be found" $ do
      startWorkingFirstWorker
      workplaceOccupants <- gets getWorkplaceOccupants <*> getWorkplace 0
      worker <- getWorker 0 0
      liftIO $ [worker] @=? workplaceOccupants
    ,
    flowTestCase "Current player is the first player in the beginning" $ do
      player <- getPlayer 0
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Just player @=? currentPlayer
    ,
    flowTestCase "Current player is second player after first move" $ do
      startWorkingFirstWorker
      player <- getPlayer 1
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Just player @=? currentPlayer
    ,
    flowTestCase "There is no current player after all moves in turn" $ do
      makeAllMovesInTurn1
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Nothing @=? currentPlayer
    ,
    flowTestCase "Current player is first player after finishing first turn" $ do
      makeAllMovesInTurn1
      apply finishTurn
      player <- getPlayer 0
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Just player @=? currentPlayer
    ,
    flowTestCase "First player can move after finishing first turn" $ do
      makeAllMovesInTurn1
      apply finishTurn
      startWorkingFirstWorker
    ,
    flowTestCase "Initial universe has valid occupants" $ do
      players <- getPlayers <$> get
      forM_ players $ \pl -> do
        errors <- getOccupantErrors <$> get <*> pure pl
        liftIO $ [] @=? errors
    ,
    flowTestCase "Altering to invalid occupants prevents ending turn" $ do
      breakOccupantsOfPlayer1
      startWorkingFirstWorker
      currentPlayer <- gets getCurrentPlayer
      pl <- player1
      liftIO $ Just pl @=? currentPlayer
    ,
    flowTestCase "Fixing occupants finishes turn" $ do
      breakOccupantsOfPlayer1
      startWorkingFirstWorker
      fixOccupantsOfPlayer1
      pl <- player2
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Just pl @=? currentPlayer
    ,
    flowTestCaseFailure "Player waiting for fixing occupants cannot move other worker" $ do
      breakOccupantsOfPlayer1
      startWorkingFirstWorker
      worker <- getWorker 0 1
      workplace <- getWorkplace 1
      apply $ startWorking worker workplace
    ,
    flowTestCase "Seventh workplace is cut forest" $ do
      workplaceId <- getWorkplace 6
      workplace <- gets ((M.! workplaceId) . getWorkplaces)
      liftIO $ CutForest @=? workplace
    ,
    flowTestCase "Starting working in cut forest gets the player to cutting forest status" $ do
      startCuttingForest
      status <- getPlayerStatus <$> get <*> player1
      liftIO $ CuttingForest @=? status
    ,
    flowTestCase "Selecting a position when cutting forest cuts forest" $ do
      startCuttingForest
      apply $ selectPosition (0, 0) DirectionDown
      buildings <- getBuildingSpace <$> get <*> player1
      liftIO $ assertBool "No grass in (0, 0)" $ Grass (0, 0) `elem` buildings
      liftIO $ assertBool "No grass in (0, 1)" $ Grass (0, 1) `elem` buildings
      return ()
    ,
    flowTestCaseFailure "Selecting an invalid position when cutting forest is not allowed" $ do
      startCuttingForest
      apply $ selectPosition (5, 5) DirectionUp
    ,
    flowTestCaseFailure "Selecting position when not cutting forest fails" $
      apply $ selectPosition (0, 0) DirectionDown
    ,
    flowTestCase "Selecting a position when cutting forest ends turn" $ do
      startCuttingForest
      apply $ selectPosition (0, 0) DirectionDown
      currentPlayer <- gets getCurrentPlayer
      pl <- player2
      liftIO $ Just pl @=? currentPlayer
    ,
    flowTestCaseFailure "Cancelling selection when not cutting forest causes error" $
      apply cancelSelection
    ,
    flowTestCase "Cancelling selection returns worker" $ do
      startCuttingForest
      apply cancelSelection
      workplace <- getWorkerWorkplace <$> get <*> getWorker 0 0
      liftIO $ Nothing @=? workplace
    ,
    flowTestCase "Cancelling selection sets player status to moving worker" $ do
      startCuttingForest
      apply cancelSelection
      status <- getPlayerStatus <$> get <*> getPlayer 0
      liftIO $ MovingWorker @=? status
    ,
    flowTestCase "Cancelling selection allows him to move worker again" $ do
      worker <- getWorker 0 0
      workplace <- getWorkplace 6
      apply $ startWorking worker workplace
      apply cancelSelection
      apply $ startWorking worker workplace
    ,
    flowTestCase "Cancelling selection keeps occupant changes" $ do
      startCuttingForest
      player <- player1
      apply $ alterOccupants player M.empty
      apply cancelSelection
      occupants <- getBuildingOccupants <$> get <*> player1
      liftIO $ M.empty @=? occupants
    ,
    flowTestCase "Cancelling selection keeps other workers" $ do
      applyStartWorking 0 0 0
      buildInFirstFreeSpace =<< getPlayer 0
      applyStartWorking 1 0 1
      buildInFirstFreeSpace =<< getPlayer 1
      applyStartWorking 0 1 6
      apply cancelSelection
      expectedWorkplace <- getWorkplace 0
      workplace <- getWorkerWorkplace <$> get <*> getWorker 0 0
      liftIO $ Just expectedWorkplace @=? workplace
    ,
    flowTestCase "Initial player has zero wood" $ do
      resources <- getPlayerResources <$> get <*> player1
      let wood = resources ^. woodAmount
      liftIO $ 0 @=? wood
  ]

breakOccupantsOfPlayer1 = do
  workers <- getWorkers <$> get <*> player1
  pl <- player1
  apply $ alterOccupants pl (M.fromList [((1, 1), [WorkerOccupant (workers !! 1)])])

fixOccupantsOfPlayer1 = do
  workers <- getWorkers <$> get <*> player1
  pl <- player1
  apply $ alterOccupants pl (M.fromList [((3, 3), [WorkerOccupant (workers !! 0), WorkerOccupant (workers !! 1)])])

startWorkingFirstWorker = do
  workplace <- getWorkplace 0
  worker <- getWorker 0 0
  apply $ startWorking worker workplace
  buildInFirstFreeSpace =<< getPlayer 0

startCuttingForest = do
  workplaceId <- getWorkplace 6
  worker <- getWorker 0 0
  apply $ startWorking worker workplaceId

applyStartWorking playerNum workerNum workplaceNum = do
  worker <- getWorker playerNum workerNum
  workplace <- getWorkplace workplaceNum
  apply $ startWorking worker workplace

makeAllMovesInTurn1 :: FlowTest Universe ()
makeAllMovesInTurn1 = do
  universe <- get
  let players = getPlayers universe
      getWorkersWithPlayer player = (player, ) <$> getWorkers universe player
      workers = (concat . transpose) (getWorkersWithPlayer <$> players)
      workplaces = keys $ getWorkplaces universe
      makeSingleTurn :: ((PlayerId, WorkerId), WorkplaceId) -> FlowTest Universe ()
      makeSingleTurn ((player, worker), workplace) = do
        apply (startWorking worker workplace)
        buildInFirstFreeSpace player
  forM_ (zip workers workplaces) makeSingleTurn

buildInFirstFreeSpace :: PlayerId -> FlowTest Universe ()
buildInFirstFreeSpace player = do
  let isFreeSpace :: [Building] -> Position -> Bool
      isFreeSpace buildings position = elem (Forest position) buildings
      getFreeSpace :: [Building] -> Position
      getFreeSpace buildings =
        let candidates = [(0, 0), (1, 0), (2, 0), (0, 2), (1, 2), (2, 2)]
        in head $ Prelude.filter (isFreeSpace buildings) candidates
  buildingSpace <- gets getBuildingSpace <*> pure player
  apply $ selectPosition (getFreeSpace buildingSpace) DirectionDown