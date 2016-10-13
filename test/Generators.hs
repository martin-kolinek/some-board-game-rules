{-# LANGUAGE RankNTypes #-}
module Generators where

import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Map.Strict (fromList, fromListWith, keys, (!), Map)
import qualified Data.Set as S
import Control.Monad (forM, foldM)
import Text.Show.Pretty
import Data.List.Split (splitPlaces, chunksOf)
import Control.Lens ((^.))
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
    workerIds <- shuffle $ WorkerId <$> [1..playerCount * 7]
    allWorkplaceIds <- shuffle $ WorkplaceId <$> [1..playerCount * 10]
    currentPlayerId : otherPlayerIds <- shuffle $ PlayerId <$> [1..playerCount]
    let (playerWorkplaceIds, additionalWorkplaceIds) = splitAt (playerCount * 7) allWorkplaceIds
    additionalWorkplaceData <- mapM (const generateWorkplaceData) additionalWorkplaceIds
    otherPlayersDone <- elements [True, False]
    let additionalWorkplaces = fromList $ zip additionalWorkplaceIds additionalWorkplaceData
        currentAvailableWorkerIds : otherAvailableWorkerIds = chunksOf 7 workerIds
        currentAvailableWorkplaceIds : otherAvailableWorkplaceIds = chunksOf 7 playerWorkplaceIds
        currentPlayerAvailableStatuses = (if otherPlayersDone then [const AllWorkersBusyStatus] else []) ++
          ((NormalStatus .) <$> [const MovingWorker,
            const OccupantsInvalid,
            const CuttingForest,
            const DiggingPassage,
            const DiggingCave,
            MakingDecision . WorkerNeedDecision,
            const $ MakingDecision CaveOrPassageDecision,
            const BuildingLivingRoom])
        otherPlayerAvailableStatuses = if otherPlayersDone then [const AllWorkersBusyStatus] else [const $ NormalStatus Waiting]
    otherPlayersGenerated <- forM (zip otherPlayerIds (zip otherAvailableWorkerIds otherAvailableWorkplaceIds)) $
      \(generatedPlayerId, (availableWorkerIds, availableWorkplaceIds)) ->
        generatePlayer generatedPlayerId availableWorkerIds availableWorkplaceIds otherPlayerAvailableStatuses
    let otherPlayerData = fst <$> otherPlayersGenerated
        otherPlayerWorkplaces = snd <$> otherPlayersGenerated
    (currentPlayerData, currentPlayerWorkplaces) <- generatePlayer currentPlayerId currentAvailableWorkerIds currentAvailableWorkplaceIds currentPlayerAvailableStatuses
    let allPlayers = fromList $ fmap (\x -> (_playerId x, x)) (currentPlayerData : otherPlayerData)
        allWorkplaces = additionalWorkplaces <> currentPlayerWorkplaces <> mconcat otherPlayerWorkplaces
    selectedStartingPlayer <- elements $ keys allPlayers
    return $ ArbitraryUniverse $ Universe allWorkplaces allPlayers selectedStartingPlayer

data GeneratedPlayerStatus = NormalStatus PlayerStatus | AllWorkersBusyStatus deriving (Show, Eq)

extractPlayerStatus :: GeneratedPlayerStatus -> PlayerStatus
extractPlayerStatus (NormalStatus s) = s
extractPlayerStatus _ = Waiting

generatePlayer :: PlayerId -> [WorkerId] -> [WorkplaceId] -> [WorkplaceId -> GeneratedPlayerStatus] -> Gen (PlayerData, Map WorkplaceId WorkplaceData)
generatePlayer generatedPlayerId availableWorkerIds availableWorkplaceIds possibleStatusFunctions = do
  let currentWorkplaceId = head availableWorkplaceIds
  selectedStatus <- elements $ possibleStatusFunctions <*> pure currentWorkplaceId
  generatedBuildingSpace@(BuildingSpace buildings) <- generateBuildingSpace
  let supportedWorkerCount = sum $ buildingSupportedWorkers <$> buildings
  totalWorkerCount <- choose (1, min 5 supportedWorkerCount)
  alreadyBusyWorkerCount <- choose (if selectedStatus == AllWorkersBusyStatus then totalWorkerCount else 0, totalWorkerCount)
  let allWorkerIds = take totalWorkerCount availableWorkerIds
      allWorkplaceIds = take totalWorkerCount availableWorkplaceIds
      alreadyBusyWorkplaceIds = take alreadyBusyWorkerCount (tail allWorkplaceIds)
  alreadyBusyWorkplaceData <- mapM (const generateWorkplaceData) [1..alreadyBusyWorkerCount]
  freeWorkplaceData <- mapM (const generateWorkplaceData) [1..totalWorkerCount - alreadyBusyWorkerCount - 1]
  currentWorkplaceData <- case selectedStatus of
    NormalStatus MovingWorker -> generateWorkplaceData
    NormalStatus Waiting -> generateWorkplaceData
    NormalStatus OccupantsInvalid -> generateWorkplaceData
    NormalStatus CuttingForest -> generateCutForest
    NormalStatus DiggingPassage -> oneof [generateDigCave, generateDigPassage]
    NormalStatus DiggingCave -> generateDigCave
    NormalStatus (MakingDecision (WorkerNeedDecision _)) -> elements [WorkerNeed]
    NormalStatus (MakingDecision CaveOrPassageDecision) -> generateDigCave
    NormalStatus BuildingLivingRoom -> elements [WorkerNeed]
    AllWorkersBusyStatus -> generateWorkplaceData
  let alreadyBusyWorkerStates = WorkerState . Just <$> alreadyBusyWorkplaceIds
      currentWorkerState = WorkerState $ case selectedStatus of
        NormalStatus MovingWorker -> Nothing
        NormalStatus Waiting -> Nothing
        _ -> Just currentWorkplaceId
      freeWorkerStates = repeat $ WorkerState Nothing
      allWorkerStates = alreadyBusyWorkerStates ++ (currentWorkerState : freeWorkerStates)
      allWorkplaceData = (currentWorkplaceData : alreadyBusyWorkplaceData) ++ freeWorkplaceData
      workplaceData = fromList $ zip allWorkplaceIds allWorkplaceData
  generatedOccupants <- if selectedStatus == NormalStatus OccupantsInvalid
                        then generateInvalidOccupants allWorkerIds
                        else generateOccupants allWorkerIds generatedBuildingSpace
  generatedResources <- generateResources
  let playerData = PlayerData
                     generatedPlayerId
                     (fromList $ zip allWorkerIds allWorkerStates)
                     generatedBuildingSpace
                     generatedOccupants
                     (extractPlayerStatus selectedStatus)
                     generatedResources
                     initialAnimals
  return (playerData, workplaceData)
