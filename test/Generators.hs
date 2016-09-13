module Generators where

import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Map.Strict (fromList, Map, keys, fromListWith, lookup)
import Data.List ((\\))
import Control.Monad (forM, join, foldM)

import Universe hiding (players)
import Workplace
import Worker
import Player hiding (playerId, workers, playerResources)
import Building
import Resources

newtype ArbitraryUniverse = ArbitraryUniverse Universe

generateWorkplaceData :: Gen WorkplaceData
generateWorkplaceData = do
  wood <- arbitrarySizedNatural
  return $ CutForest wood

generateWorkplaces :: Int -> Gen (Map WorkplaceId WorkplaceData)
generateWorkplaces minNumber = do
  neededLst <- vectorOf minNumber generateWorkplaceData
  lst <- listOf generateWorkplaceData
  let ids = WorkplaceId <$> [1..]
  return $ fromList (zip ids (neededLst ++ lst))

generateBuildingSpace :: Gen BuildingSpace
generateBuildingSpace = do
  cutForestCount <- choose (0, 12) :: Gen Int
  let findNextCandidates (sx, sy) = filter isValid [(sx+1, sy), (sx-1, sy), (sx, sy+1), (sx, sy-1)]
        where isValid (x, y) = x >=0 && x <=2 && y >= 0 && y <= 3
      expand (candidates, current) _ = do
        chosen <- elements candidates
        return ((candidates \\ [chosen]) ++ findNextCandidates chosen, chosen : current)
  (_, cutPositions) <- foldM expand ([(2, 3)], []) [1..cutForestCount]
  forestBuildings <- forM cutPositions $ \position ->
    elements [Field position, Grass position]
  let rocks = [Rock (x, y) | x <- [3..5], y <- [0..3], (x, y) /= (3, 3), (x, y) /= (3, 2)]
      initialRoom = [InitialRoom (3, 3), InitialRoom (3, 2)]
  return $ BuildingSpace (forestBuildings ++ rocks ++ initialRoom)

generateValidOccupants :: [WorkerId] -> Gen BuildingOccupants
generateValidOccupants workerIds = do
  shuffled <- shuffle workerIds
  let (position1, position2) = splitAt 2 (WorkerOccupant <$> shuffled)
  return $ fromList $ [((3, 3), position1), ((3, 4), position2)]

generateInvalidOccupants :: [WorkerId] -> Gen BuildingOccupants
generateInvalidOccupants workerIds = do
  shuffled <- shuffle workerIds
  positions <- infiniteListOf $ do
    x <- choose (0, 2) :: Gen Int
    y <- choose (0, 3) :: Gen Int
    return (x, y)
  let positionsWithWorkers = zip positions (return . WorkerOccupant <$> shuffled)
  return $ fromListWith (++) positionsWithWorkers

generateOccupants :: [WorkerId] -> Gen BuildingOccupants
generateOccupants workers =
  oneof [generateValidOccupants workers, generateInvalidOccupants workers]

generateResources :: Gen Resources
generateResources = Resources
  <$> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)

instance Arbitrary ArbitraryUniverse where
  arbitrary = do
    playerCount <- choose (1, 7) :: Gen Int
    let playerIds = PlayerId <$> [1..playerCount]
    currentPlayerId <- elements playerIds
    currentPlayerStatus <- elements [MovingWorker, OccupantsInvalid, CuttingForest, Waiting]
    currentPlayerWorkerCount <- choose (1, 4) :: Gen Int
    currentPlayerWorkers <- shuffle $ WorkerId <$> [1..currentPlayerWorkerCount]
    let (minWorkersFree, minWorkersCuttingForest) = case currentPlayerStatus of
          MovingWorker -> (1, 0)
          OccupantsInvalid -> (0, 0)
          CuttingForest -> (0, 1)
          Waiting -> (0, length currentPlayerWorkers)
    freeCurrentPlayerWorkerCount <- choose (minWorkersFree, length currentPlayerWorkers - minWorkersCuttingForest)
    let (freeCurrentPlayerWorkers, busyCurrentPlayerWorkers) = splitAt freeCurrentPlayerWorkerCount currentPlayerWorkers
    mostRecentWorkerId <- if currentPlayerStatus == CuttingForest then elements (Just <$> busyCurrentPlayerWorkers) else return Nothing
    otherPlayerData <- forM (playerIds \\ [currentPlayerId]) $ \playerId@(PlayerId num) -> do
      workerCount <- choose (1, 4)
      workerIds <- shuffle $ WorkerId <$> [num*4 + 1..num*4 + workerCount]
      freeWorkerCount <- choose (if currentPlayerStatus == Waiting then workerCount else 0, workerCount)
      let (freeWorkers, busyWorkers) = splitAt freeWorkerCount workerIds
      return (playerId, freeWorkers, busyWorkers)
    let allBusyWorkers = busyCurrentPlayerWorkers ++ join ((\(_, _, x) -> x) <$> otherPlayerData)
        allFreeWorkers = freeCurrentPlayerWorkers ++ join ((\(_, x, _) -> x) <$> otherPlayerData)
        allWorkers = allBusyWorkers ++ allFreeWorkers
    workplaces <- generateWorkplaces (length allWorkers)
    shuffledWorkplaceIds <- shuffle $ keys workplaces
    let workersWithWorkplaces = fromList $ zip allBusyWorkers shuffledWorkplaceIds
    currentPlayerBuildingSpace <- generateBuildingSpace
    currentPlayerOccupants <- if currentPlayerStatus == OccupantsInvalid then generateInvalidOccupants currentPlayerWorkers else generateOccupants currentPlayerWorkers
    currentPlayerResources <- generateResources
    let currentPlayerData =
          (currentPlayerId, PlayerData
                       currentPlayerId
                       (fromList [(workerId, WorkerState $ lookup workerId workersWithWorkplaces) | workerId <- currentPlayerWorkers])
                       currentPlayerBuildingSpace
                       currentPlayerOccupants
                       currentPlayerStatus
                       mostRecentWorkerId
                       currentPlayerResources)
    otherPlayers <- forM (playerIds \\ [currentPlayerId]) $ \playerId -> do
      let playerWorkers = [workerId | (plId, busyWorkers, freeWorkers) <- otherPlayerData, plId == playerId, workerId <- busyWorkers ++ freeWorkers]
      playerBuildingSpace <- generateBuildingSpace
      playerOccupants <- generateOccupants playerWorkers
      playerResources <- generateResources
      return $ (playerId, PlayerData
                            playerId
                            (fromList [(workerId, WorkerState $ lookup workerId workersWithWorkplaces) | workerId <- playerWorkers])
                            playerBuildingSpace
                            playerOccupants
                            Waiting
                            Nothing
                            playerResources)
    let players = fromList $ otherPlayers ++ [currentPlayerData]
    return $ ArbitraryUniverse $ Universe workplaces players
