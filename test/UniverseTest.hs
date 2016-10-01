{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, ImpredicativeTypes #-}

module UniverseTest where

import Data.Map         as M
import Test.Tasty
import Test.Tasty.HUnit as H
import Data.List
import Control.Monad
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
    flowTestCase initialUniverse "Current player is the first player in the beginning" $ do
      player <- getPlayer 0
      currentPlayer <- gets getCurrentPlayer
      liftIO $ Just player @=? currentPlayer
    ,
    flowTestCase initialUniverse "Initial universe has valid occupants" $ do
      players <- getPlayers <$> get
      forM_ players $ \pl -> do
        errors <- getOccupantErrors <$> get <*> pure pl
        liftIO $ [] @=? errors
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
    flowTestCaseFailure initialUniverse "Selecting an invalid position when cutting forest is not allowed" $ do
      startCuttingForest
      apply $ selectPosition (5, 5) DirectionUp
    ,
    flowTestCaseFailure initialUniverse "Selecting position when not cutting forest fails" $
      apply $ selectPosition (0, 0) DirectionDown
    ,
    flowTestCaseFailure initialUniverse "Canceling selection when not cutting forest causes error" $
      apply cancelSelection
    ,
    flowTestCase initialUniverse "Initial player has zero wood" $ do
      resources <- getPlayerResources <$> get <*> player1
      let wood = resources ^. woodAmount
      liftIO $ 0 @=? wood
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
