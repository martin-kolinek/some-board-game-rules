module Generators where

import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Map.Strict (fromList, fromListWith, lookup, union)
import qualified Data.Set as S
import Data.List ((\\))
import Control.Monad (forM, join, foldM)
import Text.Show.Pretty

import Universe hiding (players)
import Workplace
import Worker
import Player hiding (playerId, workers, playerResources)
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

generateWorkplaceData :: Gen WorkplaceData
generateWorkplaceData = oneof [generateDigPassage, generateCutForest, generateDigCave, elements [WorkerNeed], elements [ResourceAddition]]

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
    currentPlayerOccupants <- if currentPlayerStatus == OccupantsInvalid then generateInvalidOccupants currentPlayerWorkers else generateOccupants currentPlayerWorkers
    currentPlayerResources <- generateResources
    let currentPlayerData =
          (currentPlayerId, PlayerData
                       currentPlayerId
                       (fromList [(workerId, WorkerState $ lookup workerId allWorkersWithWorkplaces) | workerId <- currentPlayerWorkers])
                       currentPlayerBuildingSpace
                       currentPlayerOccupants
                       (updateStatus currentPlayerStatus)
                       currentPlayerResources)
    otherPlayers <- forM (playerIds \\ [currentPlayerId]) $ \playerId -> do
      let playerWorkers = [workerId | (plId, busyWorkers, freeWorkers) <- otherPlayerData, plId == playerId, workerId <- busyWorkers ++ freeWorkers]
      playerBuildingSpace <- generateBuildingSpace
      playerOccupants <- generateOccupants playerWorkers
      playerResources <- generateResources
      return $ (playerId, PlayerData
                            playerId
                            (fromList [(workerId, WorkerState $ lookup workerId allWorkersWithWorkplaces) | workerId <- playerWorkers])
                            playerBuildingSpace
                            playerOccupants
                            Waiting
                            playerResources)
    let players = fromList $ otherPlayers ++ [currentPlayerData]
    return $ ArbitraryUniverse $ Universe (fromList workplaces) players
