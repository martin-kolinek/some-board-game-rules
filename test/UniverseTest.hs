{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, ImpredicativeTypes #-}

module UniverseTest where

import Data.Map         as M
import Test.Tasty
import Test.Tasty.HUnit as H
import Data.List
import Control.Monad
import Data.Maybe
import TestFramework
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens hiding (universe)

import Workplace
import Worker
import Building
import Player hiding (workers, playerId, buildingSpace)
import Resources
import Universe hiding (players)
import Universe.Player
import Universe.Worker
import Universe.Workplace
import Universe.Actions
import Universe.Building

universeTests :: TestTree
universeTests = testGroup "Universe" [
    flowTestCase initialUniverse "Initial universe has two players" $ do
      universe <- get
      liftIO $ 2 @=? (length . getPlayers) universe
    ,
    flowTestCase initialUniverse "Initial universe has two workers for player 1" $ do
      workers <- getWorkers <$> get <*> player1
      liftIO $ 2 @=? length workers
    ,
    flowTestCase initialUniverse "Initial universe has three worker for player 2" $ do
      workers <- getWorkers <$> get <*> player2
      liftIO $ 3 @=? length workers
    ,
    flowTestCase initialUniverse "Initial universe has six increase score workplaces" $ do
      workplaces <- getWorkplaces <$> get
      liftIO $ replicate 12 (CutForest 3) @=? elems workplaces
    ,
    flowTestCaseFailure initialUniverse "Start working in invalid workplace causes error" $ do
      worker <- getWorker 0 0
      _ <- startWorking worker (WorkplaceId 50) =<< get
      return ()
    ,
    flowTestCaseFailure initialUniverse "Start working by invalid worker causes error" $ do
      workplace <- getWorkplace 0
      _ <- startWorking (WorkerId 50) workplace =<< get
      return ()
    ,
    flowTestCase initialUniverse "Start working assigns worker" $ do
      startWorkingFirstWorker
      workerWorkplace <- getWorkerWorkplace <$> get <*> getWorker 0 0
      firstWorkplace <- getWorkplace 0
      liftIO $ Just firstWorkplace @=? workerWorkplace
    ,
    flowTestCase initialUniverse "Finishing turn unassigns workers" $ do
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
    flowTestCaseFailure initialUniverse "Finishing turn is not possible without assigned workers" $
      apply finishTurn
    ,
    flowTestCaseFailure initialUniverse "Moving the same player twice causes error" $ do
      worker1 <- getWorker 0 0
      workplace1 <- getWorkplace 0
      workplace2 <- getWorkplace 2
      apply $ startWorking worker1 workplace1
      apply $ startWorking worker1 workplace2
    ,
    flowTestCase initialUniverse "Workplace occupants can be found" $ do
      startWorkingFirstWorker
      workplaceOccupants <- gets getWorkplaceOccupants <*> getWorkplace 0
      worker <- getWorker 0 0
      liftIO $ [worker] @=? workplaceOccupants
    ,
    flowTestCase initialUniverse "Current player is the first player in the beginning" $ do
      player <- getPlayer 0
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Just player @=? currentPlayer
    ,
    flowTestCase initialUniverse "Current player is second player after first move" $ do
      startWorkingFirstWorker
      player <- getPlayer 1
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Just player @=? currentPlayer
    ,
    flowTestCase initialUniverse "There is no current player after all moves in turn" $ do
      makeAllMovesInTurn1
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Nothing @=? currentPlayer
    ,
    flowTestCase initialUniverse "Current player is first player after finishing first turn" $ do
      makeAllMovesInTurn1
      apply finishTurn
      player <- getPlayer 0
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Just player @=? currentPlayer
    ,
    flowTestCase initialUniverse "First player can move after finishing first turn" $ do
      makeAllMovesInTurn1
      apply finishTurn
      startWorkingFirstWorker
    ,
    flowTestCase initialUniverse "Initial universe has valid occupants" $ do
      players <- getPlayers <$> get
      forM_ players $ \pl -> do
        errors <- getOccupantErrors <$> get <*> pure pl
        liftIO $ [] @=? errors
    ,
    flowTestCase initialUniverse "Altering to invalid occupants prevents ending turn" $ do
      breakOccupantsOfPlayer1
      startWorkingFirstWorker
      currentPlayer <- gets getCurrentPlayer
      pl <- player1
      liftIO $ Just pl @=? currentPlayer
    ,
    flowTestCase initialUniverse "Fixing occupants finishes turn" $ do
      breakOccupantsOfPlayer1
      startWorkingFirstWorker
      fixOccupantsOfPlayer1
      pl <- player2
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Just pl @=? currentPlayer
    ,
    flowTestCaseFailure initialUniverse "Player waiting for fixing occupants cannot move other worker" $ do
      breakOccupantsOfPlayer1
      startWorkingFirstWorker
      worker <- getWorker 0 1
      workplace <- getWorkplace 1
      apply $ startWorking worker workplace
    ,
    flowTestCase initialUniverse "Seventh workplace is cut forest" $ do
      workplaceId <- getWorkplace 6
      workplace <- gets ((M.! workplaceId) . getWorkplaces)
      liftIO $ (CutForest 3) @=? workplace
    ,
    flowTestCase initialUniverse "Starting working in cut forest gets the player to cutting forest status" $ do
      startCuttingForest
      status <- getPlayerStatus <$> get <*> player1
      liftIO $ CuttingForest @=? status
    ,
    flowTestCase initialUniverse "Selecting a position when cutting forest cuts forest" $ do
      startCuttingForest
      apply $ selectPosition (0, 0) DirectionDown
      buildings <- getBuildingSpace <$> get <*> player1
      liftIO $ assertBool "No grass in (0, 0)" $ Grass (0, 0) `elem` buildings
      liftIO $ assertBool "No grass in (0, 1)" $ Field (0, 1) `elem` buildings
      return ()
    ,
    flowTestCaseFailure initialUniverse "Selecting an invalid position when cutting forest is not allowed" $ do
      startCuttingForest
      apply $ selectPosition (5, 5) DirectionUp
    ,
    flowTestCaseFailure initialUniverse "Selecting position when not cutting forest fails" $
      apply $ selectPosition (0, 0) DirectionDown
    ,
    flowTestCase initialUniverse "Selecting a position when cutting forest ends turn" $ do
      startCuttingForest
      apply $ selectPosition (0, 0) DirectionDown
      currentPlayer <- gets getCurrentPlayer
      pl <- player2
      liftIO $ Just pl @=? currentPlayer
    ,
    flowTestCaseFailure initialUniverse "Canceling selection when not cutting forest causes error" $
      apply cancelSelection
    ,
    flowTestCase initialUniverse "Initial player has zero wood" $ do
      resources <- getPlayerResources <$> get <*> player1
      let wood = resources ^. woodAmount
      liftIO $ 0 @=? wood
    ,
    flowTestCase initialUniverse "After selecting position wood is assigned to player" $ do
      applyStartWorking 0 0 0
      buildInFirstFreeSpace =<< getPlayer 0
      resources <- getPlayerResources <$> get <*> getPlayer 0
      let wood = resources ^. woodAmount
      liftIO $ 3 @=? wood
    ,
    flowTestCase initialUniverse "After selecting position wood is removed from workplace" $ do
      applyStartWorking 0 0 0
      buildInFirstFreeSpace =<< getPlayer 0
      workplaceId <- getWorkplace 0
      workplaceMap <- gets getWorkplaces
      liftIO $ Just (CutForest 0) @=? M.lookup workplaceId workplaceMap
    ,
    flowTestCase initialUniverse "After finishing single turn wood is added to workplaces" $ do
      makeAllMovesInTurn1
      workplaceAfterMoves <- getWorkplaceData 0
      liftIO $ CutForest 0 @=? workplaceAfterMoves
      apply finishTurn
      workplaceAfterFinish <- getWorkplaceData 0
      liftIO $ CutForest 3 @=? workplaceAfterFinish
      lastWorkplaceAfterFinish <- getWorkplaceData 11
      liftIO $ CutForest 4 @=? lastWorkplaceAfterFinish
  ]

getWorkplaceData :: MonadState Universe m => Int -> m WorkplaceData
getWorkplaceData workplaceNum = do
  workplaceId <- getWorkplace workplaceNum
  workplaceMap <- gets getWorkplaces
  return $ workplaceMap M.! workplaceId

breakOccupantsOfPlayer1 :: FlowTest Universe ()
breakOccupantsOfPlayer1 = do
  workers <- getWorkers <$> get <*> player1
  pl <- player1
  apply $ alterOccupants pl (M.fromList [((1, 1), [WorkerOccupant (workers !! 1)])])

fixOccupantsOfPlayer1  :: FlowTest Universe ()
fixOccupantsOfPlayer1 = do
  workers <- getWorkers <$> get <*> player1
  pl <- player1
  apply $ alterOccupants pl (M.fromList [((3, 3), [WorkerOccupant (workers !! 0), WorkerOccupant (workers !! 1)])])

startWorkingFirstWorker :: FlowTest Universe ()
startWorkingFirstWorker = do
  workplace <- getWorkplace 0
  worker <- getWorker 0 0
  apply $ startWorking worker workplace
  buildInFirstFreeSpace =<< getPlayer 0

startCuttingForest :: FlowTest Universe ()
startCuttingForest = do
  workplaceId <- getWorkplace 6
  worker <- getWorker 0 0
  apply $ startWorking worker workplaceId

applyStartWorking :: Int -> Int -> Int -> FlowTest Universe ()
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
