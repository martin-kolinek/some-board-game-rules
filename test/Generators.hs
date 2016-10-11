{-# LANGUAGE RankNTypes #-}
module Generators where

import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Map.Strict (fromList, fromListWith, keys, (!), Map)
import Data.Maybe (isNothing)
import qualified Data.Set as S
import Control.Monad (forM, foldM)
import Text.Show.Pretty
import Data.List.Split (splitPlaces)
import Control.Monad.State
import Control.Lens (view, over, _1, _2, _3, _4, Lens', (^.), lengthOf, filtered)
import Data.Monoid ((<>))

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
  dugRockCount <- choose (0, 11) :: Gen Int
  let isValidForest (x, y) = x >=0 && x <=2 && y >= 0 && y <= 3
      isValidRock (x, y) = x >= 3 && x <= 5 && y >= 0 && y <= 3 && (x, y) /= (3, 3)
      findNextCandidates isValid (sx, sy) = S.filter isValid $ S.fromList [(sx+1, sy), (sx-1, sy), (sx, sy+1), (sx, sy-1)]
      expand isValid (candidates, current) _ = do
        chosen <- elements $ S.toList candidates
        let nextCurrent = S.insert chosen current
        let nextCandidates = (candidates `S.union` findNextCandidates isValid chosen) `S.difference` nextCurrent
        return (nextCandidates, nextCurrent)
  (_, cutPositions) <- foldM (expand isValidForest) (S.singleton (2, 3), S.empty) [1..cutForestCount]
  (_, dugPositions) <- foldM (expand isValidRock) (S.fromList [(4, 3), (3, 2)], S.empty) [1..dugRockCount]
  cutForestBuildings <- forM (S.toList cutPositions) $ \position ->
    elements [Field position, Grass position]
  dugRockBuildings <- forM (S.toList dugPositions) $ \position ->
    frequency [(1, elements [Passage position, Cave position]), (1, elements [LivingRoom position])]
  let rocks = Rock <$> S.toList (S.fromList [(x, y) | x <- [3..5], y <- [0..3], (x, y) /= (3, 3)] S.\\ dugPositions)
      initialRoom = [InitialRoom (3, 3)]
      forestBuildings = Forest <$> S.toList (S.fromList [(x, y) | x <- [0..2], y <- [0..3]] S.\\ cutPositions)
  return $ BuildingSpace (cutForestBuildings ++ forestBuildings ++ rocks ++ initialRoom ++ dugRockBuildings)

generateValidOccupants :: [WorkerId] -> BuildingSpace -> Gen BuildingOccupants
generateValidOccupants workerIds (BuildingSpace buildings) = do
  shuffledOccupants <- shuffle $ WorkerOccupant <$> workerIds
  let buildingsWithSpace = filter ((>0) . buildingSupportedWorkers) buildings
      workersPerBuilding = splitPlaces (buildingSupportedWorkers <$> buildingsWithSpace) shuffledOccupants
      positionedWorkers = zip (head . buildingPositions <$> buildingsWithSpace) workersPerBuilding
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
  frequency [(2, generateValidOccupants workers playerBuildingSpace), (1, generateInvalidOccupants workers)]

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
    ids1 <- shuffle [1..10]
    ids2 <- shuffle [1..50]
    ids3 <- shuffle [1..50]
    ids4 <- shuffle [1..80]
    (result, _) <- flip runStateT (ids1, ids2, ids3, ids4) $ do
      otherPlayersDone <- lift $ elements [True, False]
      otherPlayerData <- mapM (const $ generateWaitingPlayer otherPlayersDone) [1 .. playerCount - 1]
      currentPlayerData <- generateAnyPlayer otherPlayersDone
      let allPlayerData = currentPlayerData : otherPlayerData
          occupiedWorkplaces = mconcat $ fst <$> allPlayerData
          universePlayers = fromList $ fmap (\x -> (x ^. P.playerId, x))snd <$> allPlayerData
          freeWorkerCount = lengthOf (traverse . P.workers . traverse . currentWorkplace . filtered isNothing) universePlayers
      freeWorkplaceCount <- lift $ choose (freeWorkerCount, 50)
      freeWorkplaceData <- mapM (const (lift generateWorkplaceData)) [1..freeWorkplaceCount]
      freeWorkplaceIds <- mapM (const newWorkplaceId) [1..freeWorkplaceCount]
      let freeWorkplaces = fromList $ zip freeWorkplaceIds freeWorkplaceData
      universeStartingPlayer <- lift $ elements $ keys universePlayers
      return $ Universe (freeWorkplaces <> occupiedWorkplaces) universePlayers universeStartingPlayer
    return $ ArbitraryUniverse $ result

type GenWithIds a = StateT ([Int], [Int], [Int], [Int]) Gen a

newId :: Lens' ([Int], [Int], [Int], [Int]) [Int] -> (Int -> a) -> GenWithIds a
newId component constructor = do
  nextId <- gets (head . view component)
  modify (over component tail)
  return $ constructor nextId

newPlayerId :: GenWithIds PlayerId
newPlayerId = newId _1 PlayerId

newWorkerId :: GenWithIds WorkerId
newWorkerId = newId _2 WorkerId

newDogId :: GenWithIds DogId
newDogId = newId _3 DogId

newWorkplaceId :: GenWithIds WorkplaceId
newWorkplaceId = newId _4 WorkplaceId

generateWaitingPlayer :: Bool -> GenWithIds (Map WorkplaceId WorkplaceData, PlayerData)
generateWaitingPlayer allWorkersBusy = do
  playerId <- newPlayerId
  chosenWorkerCount <- lift $ choose (1, 5)
  chosenBusyWorkers <- lift $ choose (if allWorkersBusy then chosenWorkerCount else 0, chosenWorkerCount)
  chosenWorkerIds <- mapM (const newWorkerId) [1..chosenWorkerCount]
  chosenWorkplaceIds <- mapM (const newWorkplaceId) [1..chosenBusyWorkers]
  lift $ do
    playerBuildingSpace@(BuildingSpace buildings) <- generateBuildingSpace
    let supportedWorkers = sum $ buildingSupportedWorkers <$> buildings
        workerCount = min chosenWorkerCount supportedWorkers
        busyWorkers = min chosenBusyWorkers workerCount
        workerIds = take workerCount chosenWorkerIds
        workplaceIds = take busyWorkers chosenWorkplaceIds
    workplaceData <- mapM (const generateWorkplaceData) [1..busyWorkers]
    let busyWorkerState = WorkerState <$> Just <$> workplaceIds
        freeWorkerState = repeat $ WorkerState Nothing
        workers = fromList $ zip workerIds (busyWorkerState ++ freeWorkerState)
        occupiedWorkplaces = fromList $ zip workplaceIds workplaceData
    occupants <- generateOccupants workerIds playerBuildingSpace
    resources <- generateResources
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
  chosenWorkerCount <- lift $ choose (1, 5)
  chosenBusyWorkers <- lift $ choose (0, chosenWorkerCount)
  chosenWorkerIds <- mapM (const newWorkerId) [1..chosenWorkerCount]
  chosenWorkplaceIds <- mapM (const newWorkplaceId) [1..chosenBusyWorkers]
  lift $ do
    playerBuildingSpace@(BuildingSpace buildings) <- generateBuildingSpace
    let supportedWorkers = sum $ buildingSupportedWorkers <$> buildings
        workerCount = min chosenWorkerCount supportedWorkers
        busyWorkers = min chosenBusyWorkers workerCount
        workerIds = take workerCount chosenWorkerIds
        workplaceIds = take busyWorkers chosenWorkplaceIds
    workplaceData <- mapM (const generateWorkplaceData) [1..busyWorkers]
    let busyWorkerState = WorkerState <$> Just <$> workplaceIds
        freeWorkerState = repeat $ WorkerState Nothing
        workers = fromList $ zip workerIds (busyWorkerState ++ freeWorkerState)
        occupiedWorkplaces = fromList $ zip workplaceIds workplaceData
    occupants <- generateInvalidOccupants workerIds
    resources <- generateResources
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
  chosenWorkerCount <- lift $ choose (1, 5)
  chosenBusyWorkers <- lift $ choose (0, chosenWorkerCount - 1)
  chosenWorkerIds <- mapM (const newWorkerId) [1..chosenWorkerCount]
  chosenWorkplaceIds <- mapM (const newWorkplaceId) [1..chosenBusyWorkers]
  lift $ do
    playerBuildingSpace@(BuildingSpace buildings) <- generateBuildingSpace
    let supportedWorkers = sum $ buildingSupportedWorkers <$> buildings
        workerCount = min chosenWorkerCount supportedWorkers
        busyWorkers = min chosenBusyWorkers (workerCount - 1)
        workerIds = take workerCount chosenWorkerIds
        workplaceIds = take busyWorkers chosenWorkplaceIds
    workplaceData <- mapM (const generateWorkplaceData) [1..busyWorkers]
    let busyWorkerState = WorkerState <$> Just <$> workplaceIds
        freeWorkerState = repeat $ WorkerState Nothing
        workers = fromList $ zip workerIds (busyWorkerState ++ freeWorkerState)
        occupiedWorkplaces = fromList $ zip workplaceIds workplaceData
    occupants <- generateOccupants workerIds playerBuildingSpace
    resources <- generateResources
    return $ (occupiedWorkplaces,
              PlayerData
               playerId
               workers
               playerBuildingSpace
               occupants
               MovingWorker
               resources
               initialAnimals)

generateBusyPlayer :: GenWithIds (Map WorkplaceId WorkplaceData, PlayerData)
generateBusyPlayer = do
  playerId <- newPlayerId
  chosenWorkerCount <- lift $ choose (1, 5)
  chosenBusyWorkers <- lift $ choose (0, chosenWorkerCount - 1)
  chosenWorkerIds <- mapM (const newWorkerId) [1..chosenWorkerCount]
  chosenWorkplaceIds <- mapM (const newWorkplaceId) [1..chosenBusyWorkers]
  currentWorkplaceId <- newWorkplaceId
  lift $ do
    playerBuildingSpace@(BuildingSpace buildings) <- generateBuildingSpace
    let supportedWorkers = sum $ buildingSupportedWorkers <$> buildings
        workerCount = min chosenWorkerCount supportedWorkers
        busyWorkers = min chosenBusyWorkers (workerCount - 1)
        workerIds = take workerCount chosenWorkerIds
        workplaceIds = take busyWorkers chosenWorkplaceIds
    workplaceData <- mapM (const generateWorkplaceData) [1..busyWorkers]
    currentPlayerStatus <- elements [CuttingForest,
                                            DiggingPassage,
                                            DiggingCave,
                                            BuildingLivingRoom,
                                            MakingDecision CaveOrPassageDecision,
                                            MakingDecision (WorkerNeedDecision currentWorkplaceId)]
    currentWorkplaceData <- case currentPlayerStatus of
      CuttingForest -> generateCutForest
      DiggingPassage -> oneof [generateDigPassage, generateDigCave]
      DiggingCave -> generateDigCave
      BuildingLivingRoom -> elements [WorkerNeed]
      MakingDecision CaveOrPassageDecision -> generateDigCave
      MakingDecision (WorkerNeedDecision _) -> elements [WorkerNeed]
      _ -> generateWorkplaceData
    let busyWorkerState = (WorkerState $ Just currentWorkplaceId) : (WorkerState <$> Just <$> workplaceIds)
        freeWorkerState = repeat $ WorkerState Nothing
        workers = fromList $ zip workerIds (busyWorkerState ++ freeWorkerState)
        occupiedWorkplaces = fromList $ zip (currentWorkplaceId : workplaceIds) (currentWorkplaceData : workplaceData)
    occupants <- generateOccupants workerIds playerBuildingSpace
    resources <- generateResources
    return $ (occupiedWorkplaces,
              PlayerData
               playerId
               workers
               playerBuildingSpace
               occupants
               currentPlayerStatus
               resources
               initialAnimals)


generateAnyPlayer :: Bool -> GenWithIds (Map WorkplaceId WorkplaceData, PlayerData)
generateAnyPlayer includeWaiting = do
  let waitingAlternatives = if includeWaiting then [(1, elements [generateWaitingPlayer True])] else []
  generateFunc <- lift $ frequency $ [(1, elements [generateInvalidOccupantsPlayer]),
                                      (1, elements [generateMovingWorkerPlayer]),
                                      (10, elements [generateBusyPlayer])] ++ waitingAlternatives
  generateFunc
