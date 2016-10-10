{-# LANGUAGE RankNTypes #-}
module Generators where

import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Map.Strict (fromList, fromListWith, lookup, union, keys, (!), Map)
import qualified Data.Set as S
import Data.List ((\\))
import Control.Monad (forM, join, foldM)
import Text.Show.Pretty
import Data.List.Split (splitPlaces)
import Control.Monad.State
import Control.Lens (view, over, _1, _2, _3, _4, Lens', (^.))

import Universe hiding (players)
import qualified Universe as U
import Workplace
import Worker
import Player hiding (playerId, workers, playerResources)
import qualified Player as P
import Building
import Resources
import Universe.Player (getPlayers)

newtype ArbitraryUniverse = ArbitraryUniverse Universe

instance Show ArbitraryUniverse where
  show (ArbitraryUniverse u) = ppShow u

generateCutForest :: Gen WorkplaceData
generateCutForest = CutForest <$> choose(0, 1000)

generateDigPassage :: Gen WorkplaceData
generateDigPassage = DigPassage <$> choose (0, 1000)

generateDigCave :: Gen WorkplaceData
generateDigCave = DigCave <$> choose (0, 1000)

generateGatherWood :: Gen WorkplaceData
generateGatherWood = GatherWood <$> choose (0, 1000)

generateGatherFood :: Gen WorkplaceData
generateGatherFood = GatherFood <$> choose (0, 1000)

generateMakeStartPlayer :: Gen WorkplaceData
generateMakeStartPlayer = MakeStartPlayer <$> choose (0, 1000)

generateWorkplaceData :: Gen WorkplaceData
generateWorkplaceData = oneof [
  generateDigPassage,
  generateCutForest,
  generateDigCave,
  elements [WorkerNeed],
  elements [ResourceAddition],
  generateGatherWood,
  generateGatherFood,
  generateMakeStartPlayer]

generateWorkplaces :: Int -> Gen WorkplaceData -> Gen [(WorkplaceId, WorkplaceData)]
generateWorkplaces minNumber firstWorkplaceGen = do
  neededLst <- vectorOf (minNumber - 1) generateWorkplaceData
  lst <- listOf generateWorkplaceData
  firstWorkplace <- firstWorkplaceGen
  let ids = WorkplaceId <$> [1..]
  return $ zip ids (firstWorkplace : neededLst ++ lst)

generateBuildingSpace :: Gen BuildingSpace
generateBuildingSpace = do
  cutForestCount <- choose (0, 12) :: Gen Int
  dugRockCount <- choose (0, 10) :: Gen Int
  let isValidForest (x, y) = x >=0 && x <=2 && y >= 0 && y <= 3
      isValidRock (x, y) = x >= 3 && x <= 5 && y >= 0 && y <= 3 && (x, y) /= (3, 3) && (x, y) /= (3, 2)
      findNextCandidates isValid (sx, sy) = S.filter isValid $ S.fromList [(sx+1, sy), (sx-1, sy), (sx, sy+1), (sx, sy-1)]
      expand isValid (candidates, current) _ = do
        chosen <- elements $ S.toList candidates
        let nextCurrent = S.insert chosen current
        let nextCandidates = (candidates `S.union` findNextCandidates isValid chosen) `S.difference` nextCurrent
        return (nextCandidates, nextCurrent)
  (_, cutPositions) <- foldM (expand isValidForest) (S.singleton (2, 3), S.empty) [1..cutForestCount]
  (_, dugPositions) <- foldM (expand isValidRock) (S.fromList [(4, 3), (4, 2), (3, 1)], S.empty) [1..dugRockCount]
  cutForestBuildings <- forM (S.toList cutPositions) $ \position ->
    elements [Field position, Grass position]
  dugRockBuildings <- forM (S.toList dugPositions) $ \position ->
    frequency [(5, elements [Passage position, Cave position]), (1, elements [LivingRoom position])]
  let rocks = Rock <$> S.toList (S.fromList [(x, y) | x <- [3..5], y <- [0..3], (x, y) /= (3, 3), (x, y) /= (3, 2)] S.\\ dugPositions)
      initialRoom = [InitialRoom (3, 3), InitialRoom (3, 2)]
      forestBuildings = Forest <$> S.toList (S.fromList [(x, y) | x <- [0..2], y <- [0..3]] S.\\ cutPositions)
  return $ BuildingSpace (cutForestBuildings ++ forestBuildings ++ rocks ++ initialRoom ++ dugRockBuildings)

generateValidOccupants :: [WorkerId] -> BuildingSpace -> Gen BuildingOccupants
generateValidOccupants workerIds (BuildingSpace buildings) = do
  shuffledOccupants <- shuffle $ WorkerOccupant <$> workerIds
  let buildingsWithSpace = filter ((>0) . buildingSupportedWorkers) buildings
      workersPerBuilding = splitPlaces (buildingSupportedWorkers <$> buildingsWithSpace) shuffledOccupants
      positionedWorkers = zip (head $ buildingPositions <$> buildingsWithSpace) workersPerBuilding
  return $ fromList positionedWorkers

generateInvalidOccupants :: [WorkerId] -> Gen BuildingOccupants
generateInvalidOccupants workerIds = do
  shuffled <- shuffle workerIds
  positions <- infiniteListOf $ do
    x <- choose (0, 2) :: Gen Int
    y <- choose (0, 3) :: Gen Int
    return (x, y)
  let positionsWithWorkers = zip positions (return . WorkerOccupant <$> shuffled)
  return $ fromListWith (++) positionsWithWorkers

generateOccupants :: [WorkerId] -> BuildingSpace -> Gen BuildingOccupants
generateOccupants workers playerBuildingSpace =
  oneof [generateValidOccupants workers playerBuildingSpace, generateInvalidOccupants workers]

generateOccupantsForPlayer :: Universe -> PlayerId -> Gen BuildingOccupants
generateOccupantsForPlayer universe playerId =
  let playerData2 = (universe ^. U.players) ! playerId
      workerIds = keys (playerData2 ^. P.workers)
      playerBuildingSpace = playerData2 ^. P.buildingSpace
  in generateOccupants workerIds playerBuildingSpace

generateFullResources :: Gen Resources
generateFullResources = Resources
  <$> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)
  <*> choose (0, 1000)

generateEmptyResources :: Gen Resources
generateEmptyResources = Resources
  <$> choose (0, 2)
  <*> choose (0, 2)
  <*> choose (0, 2)
  <*> choose (0, 2)
  <*> choose (0, 2)
  <*> choose (0, 2)
  <*> choose (0, 2)
  <*> choose (0, 2)

generateResources :: Gen Resources
generateResources = oneof [generateEmptyResources, generateFullResources]

genPlayers :: Universe -> Gen PlayerId
genPlayers universe = elements $ getPlayers universe

instance Arbitrary ArbitraryUniverse where
  arbitrary = do
    playerCount <- choose (1, 7) :: Gen Int
    let playerIds = PlayerId <$> [1..playerCount]
    currentPlayerId <- elements playerIds
    currentPlayerStatus <- elements [MovingWorker,
                                     OccupantsInvalid,
                                     CuttingForest,
                                     DiggingPassage,
                                     DiggingCave,
                                     MakingDecision (WorkerNeedDecision (WorkplaceId (-1))),
                                     MakingDecision CaveOrPassageDecision,
                                     BuildingLivingRoom,
                                     Waiting]
    currentPlayerWorkerCount <- choose (1, 4) :: Gen Int
    currentPlayerWorkers <- shuffle $ WorkerId <$> [1..currentPlayerWorkerCount]
    let (minWorkersFree, minWorkersBusy) = case currentPlayerStatus of
          MovingWorker -> (1, 0)
          OccupantsInvalid -> (0, 0)
          CuttingForest -> (0, 1)
          DiggingPassage -> (0, 1)
          DiggingCave -> (0, 1)
          MakingDecision (_) -> (0, 1)
          BuildingLivingRoom -> (0, 1)
          Waiting -> (0, length currentPlayerWorkers)
    freeCurrentPlayerWorkerCount <- choose (minWorkersFree, length currentPlayerWorkers - minWorkersBusy)
    otherPlayersFinished <- frequency [(1, elements [False]), (4, elements [True])]
    let (freeCurrentPlayerWorkers, busyCurrentPlayerWorkers) = splitAt freeCurrentPlayerWorkerCount currentPlayerWorkers
    otherPlayerData <- forM (playerIds \\ [currentPlayerId]) $ \playerId@(PlayerId num) -> do
      workerCount <- choose (1, 4)
      workerIds <- shuffle $ WorkerId <$> [num*4 + 1..num*4 + workerCount]
      freeWorkerCount <- choose (0, if currentPlayerStatus == Waiting || otherPlayersFinished then 0 else workerCount)
      let (freeWorkers, busyWorkers) = splitAt freeWorkerCount workerIds
      return (playerId, freeWorkers, busyWorkers)
    let allBusyWorkers = busyCurrentPlayerWorkers ++ join ((\(_, _, x) -> x) <$> otherPlayerData)
        allFreeWorkers = freeCurrentPlayerWorkers ++ join ((\(_, x, _) -> x) <$> otherPlayerData)
        allWorkers = allBusyWorkers ++ allFreeWorkers
    workplaces <- generateWorkplaces (length allWorkers) $ case currentPlayerStatus of
      CuttingForest -> generateCutForest
      DiggingPassage -> oneof [generateDigPassage, generateDigCave]
      DiggingCave -> generateDigCave
      MakingDecision CaveOrPassageDecision -> generateDigCave
      MakingDecision (WorkerNeedDecision _) -> elements [WorkerNeed]
      BuildingLivingRoom -> elements [WorkerNeed]
      _ -> generateWorkplaceData
    let getDuplicatedWorkplaceIds (wId, WorkerNeed) = [wId, wId]
        getDuplicatedWorkplaceIds (wId, _) = [wId]
        workplaceIdsToShuffle = getDuplicatedWorkplaceIds =<< drop 1 workplaces
    shuffledWorkplaceIds <- shuffle workplaceIdsToShuffle
    let workersWithWorkplaces = fromList $ zip (drop 1 allBusyWorkers) shuffledWorkplaceIds
        firstWorkerWithWorkplace = zip (take 1 allBusyWorkers) (take 1 (fst <$> workplaces))
        allWorkersWithWorkplaces = workersWithWorkplaces `union` fromList firstWorkerWithWorkplace
        updateStatus (MakingDecision (WorkerNeedDecision _)) = MakingDecision $ WorkerNeedDecision $ head $ fst <$> workplaces
        updateStatus other = other
    currentPlayerBuildingSpace <- generateBuildingSpace
    currentPlayerOccupants <- if currentPlayerStatus == OccupantsInvalid
                              then generateInvalidOccupants currentPlayerWorkers
                              else generateOccupants currentPlayerWorkers currentPlayerBuildingSpace
    currentPlayerResources <- generateResources
    dogNumbers <- mapM (const $ choose (0, 10)) [1..playerCount]
    dogIds <- shuffle $ DogId <$> [1..sum dogNumbers]
    let dogIdsChunked = splitPlaces dogNumbers dogIds
        playerDogs = fromList $ zip playerIds dogIdsChunked
        currentPlayerData =
          (currentPlayerId, PlayerData
                       currentPlayerId
                       (fromList [(workerId, WorkerState $ lookup workerId allWorkersWithWorkplaces) | workerId <- currentPlayerWorkers])
                       currentPlayerBuildingSpace
                       currentPlayerOccupants
                       (updateStatus currentPlayerStatus)
                       currentPlayerResources
                       (Animals (playerDogs ! currentPlayerId)))
    otherPlayers <- forM (playerIds \\ [currentPlayerId]) $ \playerId -> do
      let playerWorkers = [workerId | (plId, busyWorkers, freeWorkers) <- otherPlayerData, plId == playerId, workerId <- busyWorkers ++ freeWorkers]
      playerBuildingSpace <- generateBuildingSpace
      playerOccupants <- generateOccupants playerWorkers playerBuildingSpace
      playerResources <- generateResources
      return $ (playerId, PlayerData
                            playerId
                            (fromList [(workerId, WorkerState $ lookup workerId allWorkersWithWorkplaces) | workerId <- playerWorkers])
                            playerBuildingSpace
                            playerOccupants
                            Waiting
                            playerResources
                            (Animals (playerDogs ! playerId)))
    let players = fromList $ otherPlayers ++ [currentPlayerData]
    startingPlayerId <- elements $ keys players
    return $ ArbitraryUniverse $ Universe (fromList workplaces) players startingPlayerId

type GenWithIds a = StateT (Int, Int, Int, Int) Gen a

newId :: Lens' (Int, Int, Int, Int) Int -> (Int -> a) -> GenWithIds a
newId component constructor = do
  nextId <- gets (view component)
  modify (over component (+1))
  return $ constructor nextId

newPlayerId :: GenWithIds PlayerId
newPlayerId = newId _1 PlayerId

newWorkerId :: GenWithIds WorkerId
newWorkerId = newId _2 WorkerId

newDogId :: GenWithIds DogId
newDogId = newId _3 DogId

newWorkplaceId :: GenWithIds WorkplaceId
newWorkplaceId = newId _4 WorkplaceId

generateWaitingPlayer :: GenWithIds (Map WorkplaceId WorkplaceData, PlayerData)
generateWaitingPlayer = do
  playerId <- newPlayerId
  playerBuildingSpace@(BuildingSpace buildings) <- lift generateBuildingSpace
  let supportedWorkers = sum $ buildingSupportedWorkers <$> buildings
  workerCount <- lift $ choose (1, min 5 supportedWorkers)
  busyWorkers <- lift $ choose (0, workerCount)
  workerIds <- mapM (const newWorkerId) [1..workerCount]
  workplaceIds <- mapM (const newWorkplaceId) [1..busyWorkers]
  workplaceData <- lift $ mapM (const generateWorkplaceData) [1..busyWorkers]
  let busyWorkerState = WorkerState <$> Just <$> workplaceIds
      freeWorkerState = repeat $ WorkerState Nothing
      workers = fromList $ zip workerIds (busyWorkerState ++ freeWorkerState)
      occupiedWorkplaces = fromList $ zip workplaceIds workplaceData
  occupants <- lift $ generateOccupants workerIds playerBuildingSpace
  resources <- lift generateResources
  return $ (occupiedWorkplaces,
            PlayerData
              playerId
              workers
              playerBuildingSpace
              occupants
              Waiting
              resources
              initialAnimals)

generateInvalidOccupantsPlayer :: GenWithIds (Map WorkplaceId WorkplaceData, PlayerData)
generateInvalidOccupantsPlayer = do
  playerId <- newPlayerId
  playerBuildingSpace@(BuildingSpace buildings) <- lift generateBuildingSpace
  let supportedWorkers = sum $ buildingSupportedWorkers <$> buildings
  workerCount <- lift $ choose (1, min 5 supportedWorkers)
  busyWorkers <- lift $ choose (0, workerCount)
  workerIds <- mapM (const newWorkerId) [1..workerCount]
  workplaceIds <- mapM (const newWorkplaceId) [1..busyWorkers]
  workplaceData <- lift $ mapM (const generateWorkplaceData) [1..busyWorkers]
  let busyWorkerState = WorkerState <$> Just <$> workplaceIds
      freeWorkerState = repeat $ WorkerState Nothing
      workers = fromList $ zip workerIds (busyWorkerState ++ freeWorkerState)
      occupiedWorkplaces = fromList $ zip workplaceIds workplaceData
  occupants <- lift $ generateInvalidOccupants workerIds
  resources <- lift generateResources
  return $ (occupiedWorkplaces,
            PlayerData
              playerId
              workers
              playerBuildingSpace
              occupants
              OccupantsInvalid
              resources
              initialAnimals)

generateMovingWorkerPlayer :: GenWithIds (Map WorkplaceId WorkplaceData, PlayerData)
generateMovingWorkerPlayer = do
  playerId <- newPlayerId
  playerBuildingSpace@(BuildingSpace buildings) <- lift generateBuildingSpace
  let supportedWorkers = sum $ buildingSupportedWorkers <$> buildings
  workerCount <- lift $ choose (1, min 5 supportedWorkers)
  busyWorkers <- lift $ choose (0, workerCount)
  workerIds <- mapM (const newWorkerId) [1..workerCount]
  workplaceIds <- mapM (const newWorkplaceId) [1..busyWorkers]
  workplaceData <- lift $ mapM (const generateWorkplaceData) [1..busyWorkers]
  let busyWorkerState = WorkerState <$> Just <$> workplaceIds
      freeWorkerState = repeat $ WorkerState Nothing
      workers = fromList $ zip workerIds (busyWorkerState ++ freeWorkerState)
      occupiedWorkplaces = fromList $ zip workplaceIds workplaceData
  occupants <- lift $ generateOccupants workerIds playerBuildingSpace
  resources <- lift generateResources
  return $ (occupiedWorkplaces,
            PlayerData
              playerId
              workers
              playerBuildingSpace
              occupants
              MovingWorker
              resources
              initialAnimals)
