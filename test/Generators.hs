{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module Generators where

import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Map.Strict (fromList, fromListWith, keys, (!), Map, empty, singleton, delete, findWithDefault)
import qualified Data.Set as S
import Control.Monad (forM, foldM, guard)
import Data.List.Split (splitPlaces, chunksOf)
import Control.Lens ((^.))
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Data.AdditiveGroup

import Universe hiding (players)
import qualified Universe as U
import Workplace
import Worker
import Player hiding (playerId, workers, playerResources)
import qualified Player as P
import Building
import Resources
import Universe.Player (getPlayers)
import Universe.Workplace
import Universe.Worker
import Actions
import Control.Lens hiding (elements, universe, chosen)
import Control.Arrow (second)

data GeneratorProperties = GeneratorProperties {
  workplaceProbabilities :: Map WorkplaceType Int,
  interactionProbabilities :: [(ActionInteraction, Int)],
  movingWorkerProbability :: Int,
  otherWorkersNotDoneProbability :: Int,
  unarmedWorkerProbability :: Int
  }

workplaceTypeResources :: WorkplaceType -> Gen Resources
workplaceTypeResources CutForest = wood <$> choose (0, 1000)
workplaceTypeResources DigPassage = stone <$> choose (0, 1000)
workplaceTypeResources DigCave = stone <$> choose (0, 1000)
workplaceTypeResources GatherWood = wood <$> choose (0, 1000)
workplaceTypeResources GatherFood = food <$> choose (0, 1000)
workplaceTypeResources MakeStartPlayer = food <$> choose (0, 1000)
workplaceTypeResources _ = elements [zeroV]

generateCutForest :: Gen WorkplaceData
generateCutForest = WorkplaceData CutForest . wood <$> choose(0, 1000)

generateDigPassage :: Gen WorkplaceData
generateDigPassage = WorkplaceData DigPassage . stone <$> choose (0, 1000)

generateDigCave :: Gen WorkplaceData
generateDigCave = WorkplaceData DigCave . stone <$> choose (0, 1000)

generateGatherWood :: Gen WorkplaceData
generateGatherWood = WorkplaceData GatherWood . wood <$> choose (0, 1000)

generateGatherFood :: Gen WorkplaceData
generateGatherFood = WorkplaceData GatherFood . food <$> choose (0, 1000)

generateMakeStartPlayer :: Gen WorkplaceData
generateMakeStartPlayer = WorkplaceData MakeStartPlayer . food <$> choose (0, 1000)

generateWorkplaceData :: GeneratorProperties -> Gen WorkplaceData
generateWorkplaceData properties = frequency $ createFreqEntry <$> [CutForest ..]
  where createFreqEntry wpType =
          (findWithDefault 1 wpType (workplaceProbabilities properties),
           WorkplaceData wpType <$> workplaceTypeResources wpType)

generateWorkplaces :: GeneratorProperties -> Int -> Gen WorkplaceData -> Gen [(WorkplaceId, WorkplaceData)]
generateWorkplaces properties minNumber firstWorkplaceGen = do
  neededLst <- vectorOf (minNumber - 1) (generateWorkplaceData properties)
  lst <- listOf (generateWorkplaceData properties)
  firstWorkplace <- firstWorkplaceGen
  let ids = WorkplaceId <$> [1..]
  return $ zip ids (firstWorkplace : neededLst ++ lst)

generateBuildingSpace :: Int -> Gen [Building]
generateBuildingSpace requiredWorkers = do
  cutForestCount <- choose (0, 12) :: Gen Int
  dugRockCount <- choose (requiredWorkers - 2, 11) :: Gen Int
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
    elements [Building Field position, Building Grass position]
  dugRockShuffled <- shuffle $ S.toList dugPositions
  let mandatoryRooms = Building LivingRoom <$> take (requiredWorkers - 2) dugRockShuffled
  hasAdditionalRooms <- elements [True, False]
  dugRockBuildings <- forM (drop (requiredWorkers - 2) dugRockShuffled) $ \position ->
    if hasAdditionalRooms then elements $ [Building Passage, Building Cave, Building LivingRoom] <*> pure position
                          else elements $ [Building Passage, Building Cave] <*> pure position
  let rocks = Building Rock <$> S.toList (S.fromList [(x, y) | x <- [3..5], y <- [0..3], (x, y) /= (3, 3)] S.\\ dugPositions)
      initialRoom = [Building InitialRoom (3, 3)]
      forestBuildings = Building Forest <$> S.toList (S.fromList [(x, y) | x <- [0..2], y <- [0..3]] S.\\ cutPositions)
  return $ cutForestBuildings ++ forestBuildings ++ rocks ++ initialRoom ++ dugRockBuildings ++ mandatoryRooms

generateValidOccupants :: [WorkerId] -> [DogId] -> [Building] -> Gen BuildingOccupants
generateValidOccupants workerIds dogIds buildings = do
  shuffledOccupants <- shuffle $ WorkerOccupant <$> workerIds
  let buildingsWithSpace = filter ((>0) . buildingSupportedWorkers) buildings
      workersPerBuilding = splitPlaces (buildingSupportedWorkers <$> buildingsWithSpace) shuffledOccupants
      positionedWorkers = zip (head . buildingPositions <$> buildingsWithSpace) workersPerBuilding
  dogPositions <- forM dogIds $ \_ -> (,) <$> choose (0, 5) <*> choose (0, 3)
  let positionedDogs = zip dogPositions ((pure . DogOccupant) <$> dogIds)
  return $ fromListWith (<>) (positionedWorkers ++ positionedDogs)

generateInvalidOccupants :: [WorkerId] -> Gen BuildingOccupants
generateInvalidOccupants workerIds = do
  shuffled <- shuffle workerIds
  positions <- infiniteListOf $ do
    x <- choose (0, 2) :: Gen Int
    y <- choose (0, 3) :: Gen Int
    return (x, y)
  let positionsWithWorkers = zip positions (return . WorkerOccupant <$> shuffled)
  return $ fromListWith (++) positionsWithWorkers

generateOccupants :: [WorkerId] -> [DogId] -> [Building] -> Gen BuildingOccupants
generateOccupants workers dogIds playerBuildings =
  frequency [(3, generateValidOccupants workers dogIds playerBuildings), (1, generateInvalidOccupants workers)]

generateOccupantsForPlayer :: Universe -> PlayerId -> Gen BuildingOccupants
generateOccupantsForPlayer universe playerId =
  let somePlayerData = (universe ^. U.players) ! playerId
      workerIds = keys (somePlayerData ^. P.workers)
      playerBuildings = somePlayerData ^. P.buildingSpace . buildingSpaceBuildings
      playerDogs = somePlayerData ^. P.playerAnimals . dogs
  in generateOccupants workerIds playerDogs playerBuildings

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

generatePlantedCrops :: [Building] -> Gen (Map Position PlantedCrop)
generatePlantedCrops buildings = do
  let getValidPosition (Building Field pos) = Just pos
      getValidPosition _ = Nothing
      possiblePositions = mapMaybe getValidPosition buildings
  plantedCropList <- forM possiblePositions $ \position -> do
    isEmpty <- elements [True, False]
    if isEmpty
      then return empty
      else do
        cropType <- elements [Potatoes, Wheat]
        cropCount <- choose (1 :: Int, if cropType == Potatoes then 2 else 3)
        return $ singleton position (PlantedCrop cropType cropCount)
  return $ mconcat plantedCropList

generateResources :: Gen Resources
generateResources = oneof [generateEmptyResources, generateFullResources]

genPlayers :: Universe -> Gen PlayerId
genPlayers universe = elements $ getPlayers universe

generateFastShuffledSequence :: Gen [Int]
generateFastShuffledSequence = do
  shuffled <- shuffle [1..5]
  seqs <- forM shuffled $ \x -> do
    rev <- elements [True, False]
    return $ ((20 * x) +) <$> if rev then [20, 19..1] else [1..20]
  return $ mconcat seqs

generateUniverse :: GeneratorProperties -> Gen Universe
generateUniverse properties = do
    playerCount <- choose (1, 7) :: Gen Int
    workerIds <- (fmap WorkerId) <$> generateFastShuffledSequence
    allWorkplaceIds <- (fmap WorkplaceId) <$> generateFastShuffledSequence
    dogIds <- (fmap DogId) <$> generateFastShuffledSequence
    currentPlayerId : otherPlayerIds <- shuffle $ PlayerId <$> [1..playerCount]
    let (playerWorkplaceIds, additionalWorkplaceIds) = splitAt (playerCount * 7) allWorkplaceIds
    additionalWorkplaceData <- mapM (const (generateWorkplaceData properties)) additionalWorkplaceIds
    otherPlayersDone <- frequency [(1, return True), (otherWorkersNotDoneProbability properties, return False)]
    let additionalWorkplaces = fromList $ zip additionalWorkplaceIds additionalWorkplaceData
        currentAvailableWorkerIds : otherAvailableWorkerIds = chunksOf 7 workerIds
        currentAvailableWorkplaceIds : otherAvailableWorkplaceIds = chunksOf 7 playerWorkplaceIds
        currentAvailableDogIds : otherAvailableDogIds = chunksOf 10 dogIds
        currentPlayerAvailableStatuses = if otherPlayersDone then [AllWorkersBusyStatus, NotWaitingStatus] else [NotWaitingStatus]
        otherPlayerAvailableStatuses = if otherPlayersDone then [AllWorkersBusyStatus] else [WaitingStatus]
    otherPlayersGenerated <- forM (zip otherPlayerIds (zip otherAvailableWorkerIds (zip otherAvailableWorkplaceIds otherAvailableDogIds))) $
      \(generatedPlayerId, (availableWorkerIds, (availableWorkplaceIds, availableDogIds))) ->
        generatePlayer properties generatedPlayerId availableWorkerIds availableWorkplaceIds availableDogIds otherPlayerAvailableStatuses
    let otherPlayerData = fst <$> otherPlayersGenerated
        otherPlayerWorkplaces = snd <$> otherPlayersGenerated
    (currentPlayerData, currentPlayerWorkplaces) <- generatePlayer properties
      currentPlayerId
      currentAvailableWorkerIds
      currentAvailableWorkplaceIds
      currentAvailableDogIds
      currentPlayerAvailableStatuses
    let allPlayers = fromList $ fmap (\x -> (_playerId x, x)) (currentPlayerData : otherPlayerData)
        allWorkplaces = additionalWorkplaces <> currentPlayerWorkplaces <> mconcat otherPlayerWorkplaces
    selectedStartingPlayer <- elements $ keys allPlayers
    return $ Universe allWorkplaces allPlayers selectedStartingPlayer

data GeneratedPlayerStatus = WaitingStatus | AllWorkersBusyStatus | NotWaitingStatus deriving (Show, Eq)

getActionProbability :: GeneratorProperties -> CompositeActionDefinition -> Int
getActionProbability properties (OptionalAction inner) = getActionProbability properties inner
getActionProbability properties (InteractionAction interaction _) = fromMaybe 1 $ fmap snd $ listToMaybe $ filter ((== interaction) . fst) (interactionProbabilities properties)
getActionProbability properties (ActionCombination combinationType act1 act2) = combine combinationType (getActionProbability properties act1) (getActionProbability properties act2)
  where combine AndThen p _ = p
        combine _ p1 p2 = p1 + p2

collectActions :: GeneratorProperties -> ActionDefinition -> [(Int, CompositeActionDefinition)]
collectActions properties (CompositeAction act) = createFreq <$> collectCompositeActions act
  where createFreq a = (getActionProbability properties a, a)
collectActions _ (StepsAction _) = []

collectCompositeActions :: CompositeActionDefinition -> [CompositeActionDefinition]
collectCompositeActions action@(ActionCombination combinationType act1 act2) = action : combine combinationType (collectCompositeActions act1) (collectCompositeActions act2)
  where combine AndOr acts1 acts2 = [ActionCombination AndOr a1 a2 | a1 <- acts1, a2 <- acts2] ++ (OptionalAction <$> acts1) ++ (OptionalAction <$> acts2)
        combine AndThen acts1 acts2 = [ActionCombination AndThen a1 act2 | a1 <- acts1] ++ acts2
        combine AndThenOr acts1 acts2 = [ActionCombination AndThen a1 (OptionalAction act2) | a1 <- acts1, a1 /= act1] ++ [a2 | a2 <- acts2, a2 /= act2] ++ (OptionalAction <$> acts2)
        combine Or acts1 acts2 = [ActionCombination Or act1 act2] ++ filter (/= act1) acts1 ++ filter (/= act2) acts2
collectCompositeActions action@(OptionalAction inner) = action : filter (/= inner) (collectCompositeActions inner)
collectCompositeActions action = [action]

possibleStatuses :: GeneratorProperties -> WorkplaceId -> WorkplaceType -> GeneratedPlayerStatus -> [(Int, Gen PlayerStatus)]
possibleStatuses properties workplaceId currentWorkplaceType NotWaitingStatus = [(movingWorkerProbability properties, return MovingWorker)] ++
  (second (return . (PerformingAction workplaceId)) <$> collectActions properties (workplaceAction currentWorkplaceType))
possibleStatuses _ _ _ WaitingStatus = [(1, return Waiting)]
possibleStatuses _ _ _ AllWorkersBusyStatus = [(1, return Waiting)]

generatePlayer :: GeneratorProperties -> PlayerId -> [WorkerId] -> [WorkplaceId] -> [DogId] -> [GeneratedPlayerStatus] -> Gen (PlayerData, Map WorkplaceId WorkplaceData)
generatePlayer properties generatedPlayerId availableWorkerIds availableWorkplaceIds availableDogIds possibleGeneratedStatuses = do
  let currentWorkplaceId = head availableWorkplaceIds
  selectedGeneratedStatus <- elements possibleGeneratedStatuses
  totalWorkerCount <- choose (1, 5)
  generatedBuildings <- generateBuildingSpace totalWorkerCount
  alreadyBusyWorkerCount <- choose (if selectedGeneratedStatus == AllWorkersBusyStatus then totalWorkerCount else 0, totalWorkerCount)
  let allWorkerIds = take totalWorkerCount availableWorkerIds
      allWorkplaceIds = take totalWorkerCount availableWorkplaceIds
      alreadyBusyWorkplaceIds = take alreadyBusyWorkerCount (tail allWorkplaceIds)
  alreadyBusyWorkplaceData <- mapM (const (generateWorkplaceData properties)) [1..alreadyBusyWorkerCount]
  freeWorkplaceData <- mapM (const (generateWorkplaceData properties)) [1..totalWorkerCount - alreadyBusyWorkerCount - 1]
  currentWorkplaceData <- generateWorkplaceData properties
  selectedStatus <- frequency $ possibleStatuses properties currentWorkplaceId (currentWorkplaceData ^. workplaceType) selectedGeneratedStatus
  let alreadyBusyWorkerStates = Just <$> alreadyBusyWorkplaceIds
      currentWorkerState = case (selectedGeneratedStatus, selectedStatus) of
        (AllWorkersBusyStatus, _) -> Just currentWorkplaceId
        (_, MovingWorker) -> Nothing
        (_, Waiting) -> Nothing
        _ -> Just currentWorkplaceId
      freeWorkerStates = repeat Nothing
      allWorkerWorkplaces = alreadyBusyWorkerStates ++ (currentWorkerState : freeWorkerStates)
      allWorkplaceData = (currentWorkplaceData : alreadyBusyWorkplaceData) ++ freeWorkplaceData
      workplaceData = fromList $ zip allWorkplaceIds allWorkplaceData
  allWorkerStates <- forM allWorkerWorkplaces $ \workplace -> do
    strength <- frequency $ [(1, choose (0, 15)), (unarmedWorkerProbability properties, return 0)]
    return $ WorkerState workplace strength
  generatedAnimals <- generateAnimals availableDogIds
  generatedOccupants <- generateOccupants allWorkerIds (generatedAnimals ^. dogs) generatedBuildings
  generatedResources <- generateResources
  generatedPlantedCrops <- generatePlantedCrops generatedBuildings
  let playerData = PlayerData
                     generatedPlayerId
                     (fromList $ zip allWorkerIds allWorkerStates)
                     (BuildingSpace generatedBuildings generatedOccupants generatedPlantedCrops)
                     selectedStatus
                     generatedResources
                     generatedAnimals
  return (playerData, workplaceData)

generateAnimals :: [DogId] -> Gen Animals
generateAnimals availableDogIds = do
  dogCount <- choose (0, 10)
  return $ Animals $ take dogCount availableDogIds

shrinkUniverse :: Universe -> [Universe]
shrinkUniverse universe =
  let shrunkByPlayers = [shrunk | plId <- getPlayers universe, shrunk <- tryRemovePlayer plId universe]
      shrunkByWorkers = [shrunk | workerId <- universe ^.. U.players . traverse . P.workers . to keys . traverse, shrunk <- tryRemoveWorker workerId universe]
      shrunkByWorkplaces = [shrunk | workplaceId <- keys $ getWorkplaces universe, shrunk <- tryRemoveWorkplace workplaceId universe]
  in shrunkByPlayers ++ shrunkByWorkplaces ++ shrunkByWorkers

tryRemovePlayer :: PlayerId -> Universe -> [Universe]
tryRemovePlayer plId universe =
  if has (U.players . ix plId . playerStatus . filtered (== Waiting)) universe &&
     lengthOf (U.players . traverse) universe > 2
  then [universe & U.players %~ (delete plId)]
  else []

tryRemoveWorkplace :: WorkplaceId -> Universe -> [Universe]
tryRemoveWorkplace workplaceId universe =
  if hasn't (U.players . traverse . P.workers . traverse . currentWorkplace . filtered (== Just workplaceId)) universe &&
     has (availableWorkplaces . to keys . traverse . filtered (/= workplaceId) . filtered (null . getWorkplaceOccupants universe)) universe
  then [universe & availableWorkplaces %~ (delete workplaceId)]
  else []

tryRemoveWorker :: WorkerId -> Universe -> [Universe]
tryRemoveWorker workerId universe = do
  plId <- universe ^.. U.players . to keys . traverse . filtered ((workerId `elem`) . getWorkers universe)
  guard $ lengthOf (U.players . ix plId . P.workers . traverse) universe > 2
  case getWorkerWorkplace universe workerId of
    Nothing -> return $ universe & U.players . ix plId . P.workers %~ delete workerId
    Just workplaceId -> do
      guard $ lengthOf (availableWorkplaces . traverse) universe > 2
      return $ universe &
        U.players . ix plId . P.workers %~ delete workerId &
        availableWorkplaces %~ delete workplaceId
