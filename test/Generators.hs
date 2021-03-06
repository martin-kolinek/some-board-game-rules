{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module Generators where

import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Map.Strict (fromList, fromListWith, keys, Map, empty, singleton, delete, findWithDefault, (!), elems)
import qualified Data.Set as S
import Control.Monad (forM, foldM, guard)
import Data.List.Split (splitPlaces, chunksOf)
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe, catMaybes)
import Data.AdditiveGroup
import Control.Arrow (second)

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

data StepProbabilities = StepProbabilities {
  addResourcesProbability :: Int,
  payResourcesProbability :: Int,
  collectResourcesStepProbability :: Int,
  addDogProbability :: Int,
  setStartPlayerProbability :: Int
  }

data InteractionProbabilities = InteractionProbabilities {
  plantCropsProbability :: Int,
  hireWorkerProbability :: Int,
  collectResourcesInteractionProbability :: Int,
  armWorkerProbability :: Int,
  adventureProbability :: Int,
  buildProbability :: Int,
  buildBarnProbability :: Int
  }

data GeneratorProperties = GeneratorProperties {
  workplaceProbabilities :: Map WorkplaceType Int,
  interactionProbabilities :: InteractionProbabilities,
  movingWorkerProbability :: Int,
  otherWorkersNotDoneProbability :: Int,
  unarmedWorkerProbability :: Int,
  notFullyDevelopedProbability :: Int,
  stepProbabilities :: StepProbabilities
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

generateBuildingSpace :: GeneratorProperties -> Int -> Gen [Building]
generateBuildingSpace properties requiredWorkers = do
  cutForestCount <- frequency [(1, return 12), (notFullyDevelopedProbability properties, choose (0, 12) :: Gen Int)]
  dugRockCount <- frequency [(1, return 11), (notFullyDevelopedProbability properties, choose (requiredWorkers - 2, 11) :: Gen Int)]
  let isValidForest (x, y) = x >=0 && x <=2 && y >= 0 && y <= 3
      isValidRock (x, y) = x >= 3 && x <= 5 && y >= 0 && y <= 3 && (x, y) /= (3, 3)
      findNextCandidates isValid (sx, sy) = filter isValid $ [(sx+1, sy), (sx-1, sy), (sx, sy+1), (sx, sy-1)]
      expand isValid (candidates, current, occupied) expectedCount =
        if expectedCount < S.size occupied || S.null candidates then return (candidates, current, occupied)
        else do
          chosen <- elements $ S.toList candidates
          tryDouble <- elements [True, False]
          direction <- elements allDirections
          let secondPosition = chosen ^+^ directionAddition direction
              double = tryDouble && isValid secondPosition && secondPosition `S.notMember` occupied
              item = if double then Left (chosen, direction) else Right chosen
              positions = if double then [chosen, secondPosition] else [chosen]
              nextCurrent = S.insert item current
              nextOccupied = S.union (S.fromList positions) occupied
              nextCandidates = (candidates `S.union` (S.fromList $ findNextCandidates isValid =<< positions)) `S.difference` nextOccupied
          return (nextCandidates, nextCurrent, nextOccupied)
  (_, cutItems, cutPositions) <- foldM (expand isValidForest) (S.singleton (2, 3), S.empty, S.empty) [1..cutForestCount]
  (_, dugItems, dugPositions) <- foldM (expand isValidRock) (S.fromList [(4, 3), (3, 2)], S.empty, S.empty) [1..dugRockCount]
  cutForestBuildings <- fmap mconcat $ forM (S.toList cutItems) $ \item -> case item of
    Right position -> (:[]) <$> elements [SmallBuilding Field position, SmallBuilding Grass position, SmallBuilding SmallPasture position]
    Left (position, direction) -> oneof [
      return $ [LargeBuilding LargePasture position direction],
      (\tp -> [SmallBuilding tp position, SmallBuilding tp (position ^+^ directionAddition direction)]) <$> elements [Field, Grass, SmallPasture]
      ]
  dugRockShuffled <- shuffle $ S.toList dugItems
  hasAdditionalRooms <- elements [True, False]
  let placeCaveBuilding (buildings, n) item
        | n <= 0 = do
            let positions = case item of
                  Right position -> [position]
                  Left (position, direction) -> [position, position ^+^ directionAddition direction]
            newBuildings <- forM positions $ \position ->
              if hasAdditionalRooms
              then elements $ [SmallBuilding Passage, SmallBuilding Cave, SmallBuilding LivingRoom] <*> pure position
              else elements $ [SmallBuilding Passage, SmallBuilding Cave] <*> pure position
            return (newBuildings ++ buildings, 0)
        | otherwise = do
            let positions = case item of
                  Right position -> [position]
                  Left (position, direction) -> [position, position ^+^ directionAddition direction]
                newBuildings = SmallBuilding LivingRoom <$>  positions
            return (newBuildings ++ buildings, n - length newBuildings)
  (dugRockBuildings, _) <- foldM placeCaveBuilding ([], requiredWorkers - 2) dugRockShuffled
  let rocks = SmallBuilding Rock <$> S.toList (S.fromList [(x, y) | x <- [3..5], y <- [0..3], (x, y) /= (3, 3)] S.\\ dugPositions)
      initialRoom = [SmallBuilding InitialRoom (3, 3)]
      forestBuildings = SmallBuilding Forest <$> S.toList (S.fromList [(x, y) | x <- [0..2], y <- [0..3]] S.\\ cutPositions)
  return $ cutForestBuildings ++ forestBuildings ++ rocks ++ initialRoom ++ dugRockBuildings

generateValidOccupants :: [WorkerId] -> [AnimalId] -> [Building] -> Gen BuildingOccupants
generateValidOccupants workerIds availableAnimalIds buildings = do
  shuffledOccupants <- shuffle $ WorkerOccupant <$> workerIds
  let buildingsWithSpace = filter ((>0) . buildingSupportedWorkers) buildings
      workersPerBuilding = splitPlaces (buildingSupportedWorkers <$> buildingsWithSpace) shuffledOccupants
      positionedWorkers = zip (head . buildingPositions <$> buildingsWithSpace) workersPerBuilding
  animalOccupantGenerators <- forM buildings $ \building ->
    generateValidAnimalsForBuilding building
  let animalOccupants = second return <$> zipWith ($) (mconcat animalOccupantGenerators) availableAnimalIds
  return $ fromListWith (<>) (positionedWorkers ++ animalOccupants)

generateValidAnimalsForBuilding :: Building -> Gen [(AnimalId -> (Position, BuildingOccupant))]
generateValidAnimalsForBuilding building = do
  generateAnimalsNumber <- choose (0 :: Int, 1)
  position <- elements $ buildingPositions building
  if generateAnimalsNumber == 0 then do
    let allowedAnimals = case building of
          (SmallBuilding SmallPasture _) -> 2
          (LargeBuilding LargePasture _ _) -> 4
          _ -> 0
    dogAmount <- oneof [return 0, choose(0, 2)]
    dogs <- vectorOf dogAmount (return (Animal Dog))
    otherAnimalType <- elements [Nothing, Just Cow, Just Sheep]
    otherAnimals <- case otherAnimalType of
      Nothing -> return dogs
      Just Sheep -> do
        let dogSupportedSheep = if dogAmount == 0 then 0 else dogAmount + 1
        possibleSheepAmount <- choose(0, max dogSupportedSheep allowedAnimals)
        let sheepAmount = case building of
              (SmallBuilding buildingType _) -> if buildingType `elem` [Grass, SmallPasture] then possibleSheepAmount else 0
              (LargeBuilding LargePasture _ _) -> possibleSheepAmount
        vectorOf sheepAmount (return (Animal (FarmAnimalType Sheep)))
      Just Cow -> do
        cowAmount <- choose (0, allowedAnimals)
        vectorOf cowAmount (return (Animal (FarmAnimalType Cow)))
    return $ (((position,) . AnimalOccupant) .) <$> (otherAnimals ++ dogs)
  else
    return []

shuffleOccupantsForPlayer :: Universe -> PlayerId -> Gen BuildingOccupants
shuffleOccupantsForPlayer universe playerId = do
  let somePlayerData = (universe ^. U.players) ! playerId
      buildingOccupants = somePlayerData ^. buildingSpace . buildingSpaceOccupants
      occupants = return <$> mconcat $ elems buildingOccupants
  positions <- infiniteListOf ((,) <$> choose (0 :: Int, 5) <*> choose (0 :: Int, 3))
  return $ fromListWith (++) $ zip positions occupants

generateInvalidOccupants :: [WorkerId] -> Gen BuildingOccupants
generateInvalidOccupants workerIds = do
  shuffled <- shuffle workerIds
  positions <- infiniteListOf $ do
    x <- choose (0, 2) :: Gen Int
    y <- choose (0, 3) :: Gen Int
    return (x, y)
  let positionsWithWorkers = zip positions (return . WorkerOccupant <$> shuffled)
  return $ fromListWith (++) positionsWithWorkers

generateOccupants :: [WorkerId] -> [AnimalId] -> [Building] -> Gen BuildingOccupants
generateOccupants workers availableAnimalIds playerBuildings =
  frequency [(3, generateValidOccupants workers availableAnimalIds playerBuildings), (1, generateInvalidOccupants workers)]

extractAnimalsFromOccupants :: BuildingOccupants -> [Animal]
extractAnimalsFromOccupants buildingOccupants = mconcat $ extractAnimal <$> (mconcat $ elems buildingOccupants)
  where extractAnimal (AnimalOccupant animal) = [animal]
        extractAnimal _ = []

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

generateBarns :: [Building] -> Gen [Position]
generateBarns buildings = do
  let validSmallType tp = tp `elem` [Grass, Forest, SmallPasture]
      validLargeType tp = tp `elem` [LargePasture]
      buildingBarnPositions (SmallBuilding tp pos) = if validSmallType tp then [pos] else []
      buildingBarnPositions (LargeBuilding tp pos dir) = if validLargeType tp then [pos, pos ^+^ directionAddition dir] else []
      availablePositions = buildingBarnPositions =<< buildings
  barnCount <- choose (0, 2)
  shuffled <- shuffle availablePositions
  return $ take barnCount shuffled

generatePlantedCrops :: [Building] -> Gen (Map Position PlantedCrop)
generatePlantedCrops buildings = do
  let getValidPosition (SmallBuilding Field pos) = Just pos
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
    animalIds <- (fmap AnimalId) <$> generateFastShuffledSequence
    currentPlayerId : otherPlayerIds <- shuffle $ PlayerId <$> [1..playerCount]
    let (playerWorkplaceIds, additionalWorkplaceIds) = splitAt (playerCount * 7) allWorkplaceIds
    additionalWorkplaceData <- mapM (const (generateWorkplaceData properties)) additionalWorkplaceIds
    otherPlayersDone <- frequency [(1, return True), (otherWorkersNotDoneProbability properties, return False)]
    let additionalWorkplaces = fromList $ zip additionalWorkplaceIds additionalWorkplaceData
        currentAvailableWorkerIds : otherAvailableWorkerIds = chunksOf 7 workerIds
        currentAvailableWorkplaceIds : otherAvailableWorkplaceIds = chunksOf 7 playerWorkplaceIds
        currentAvailableDogIds : otherAvailableDogIds = chunksOf 10 animalIds
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

generateCompositeAction :: GeneratorProperties -> Gen CompositeActionDefinition
generateCompositeAction props = generateSizedCompositeAction =<< choose (1 :: Int, 4)
  where generateSizedCompositeAction 1 = generateInteractionAction props
        generateSizedCompositeAction n = do
          combinator <- elements [AndOr, AndThen, AndThenOr, Or]
          firstAct <- generateSizedCompositeAction (n - 1)
          secondAct <- generateSizedCompositeAction (n - 1)
          let combined = ActionCombination combinator firstAct secondAct
              optional = OptionalAction firstAct
          oneof [return combined, generateInteractionAction props, return optional]

generateInteractionAction :: GeneratorProperties -> Gen CompositeActionDefinition
generateInteractionAction properties =
  InteractionAction <$> interaction <*> steps
  where ifreq getter = getter (interactionProbabilities properties)
        interaction = frequency [(ifreq plantCropsProbability, return PlantCropsInteraction),
                                 (ifreq hireWorkerProbability, return HireWorkerInteraction),
                                 (ifreq collectResourcesInteractionProbability, return CollectResourcesInteraction),
                                 (ifreq armWorkerProbability, return ArmWorkerInteraction),
                                 (ifreq adventureProbability, return AdventureInteraction),
                                 (ifreq buildBarnProbability, return BuildBarnInteraction),
                                 (ifreq buildProbability, generateBuildingInteraction)]
        steps = do
          let genStep freq step = frequency [(1, return Nothing), (freq (stepProbabilities properties), Just <$> step)]
          setStartPlayer <- genStep setStartPlayerProbability (return SetStartPlayerStep)
          addDog <- genStep addDogProbability (return AddDogStep)
          payResources <- genStep payResourcesProbability (PayResources <$> generateResources)
          collectResources <- genStep collectResourcesStepProbability (return CollectResourcesStep)
          addResources <- genStep addResourcesProbability (AddResourcesStep <$> generateResources)
          shuffle $ catMaybes $ [addDog, payResources, setStartPlayer, collectResources, addResources]
        generateBuildingInteraction = fmap BuildBuildingsInteraction
          (elements ((SingleSmallBuildingDesc <$> S.toList (S.fromList [Forest ..] S.\\ S.fromList [Forest, Rock, InitialRoom])) ++
                     [(DoubleSmallBuildingDesc Grass Field), (DoubleSmallBuildingDesc Cave Cave), (DoubleSmallBuildingDesc Cave Passage), (LargeBuildingDesc LargePasture)]))

possibleStatuses :: GeneratorProperties -> WorkplaceId -> GeneratedPlayerStatus -> Gen PlayerStatus
possibleStatuses properties workplaceId NotWaitingStatus = frequency $
  [(movingWorkerProbability properties, return MovingWorker),
   (1, (PerformingAction workplaceId) <$> generateCompositeAction properties)]
possibleStatuses _ _ WaitingStatus = return Waiting
possibleStatuses _ _ AllWorkersBusyStatus = return Waiting

generatePlayer :: GeneratorProperties -> PlayerId -> [WorkerId] -> [WorkplaceId] -> [AnimalId] -> [GeneratedPlayerStatus] -> Gen (PlayerData, Map WorkplaceId WorkplaceData)
generatePlayer properties generatedPlayerId availableWorkerIds availableWorkplaceIds availableAnimalIds possibleGeneratedStatuses = do
  let currentWorkplaceId = head availableWorkplaceIds
  selectedGeneratedStatus <- elements possibleGeneratedStatuses
  totalWorkerCount <- choose (1, 5)
  generatedBuildings <- generateBuildingSpace properties totalWorkerCount
  alreadyBusyWorkerCount <- choose (if selectedGeneratedStatus == AllWorkersBusyStatus then totalWorkerCount else 0, totalWorkerCount)
  let allWorkerIds = take totalWorkerCount availableWorkerIds
      allWorkplaceIds = take totalWorkerCount availableWorkplaceIds
      alreadyBusyWorkplaceIds = take alreadyBusyWorkerCount (tail allWorkplaceIds)
  alreadyBusyWorkplaceData <- mapM (const (generateWorkplaceData properties)) [1..alreadyBusyWorkerCount]
  freeWorkplaceData <- mapM (const (generateWorkplaceData properties)) [1..totalWorkerCount - alreadyBusyWorkerCount - 1]
  currentWorkplaceData <- generateWorkplaceData properties
  selectedStatus <- possibleStatuses properties currentWorkplaceId selectedGeneratedStatus
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
  generatedBarns <- generateBarns generatedBuildings
  generatedOccupants <- generateOccupants allWorkerIds availableAnimalIds generatedBuildings
  generatedResources <- generateResources
  generatedPlantedCrops <- generatePlantedCrops generatedBuildings
  let playerData = PlayerData
                     generatedPlayerId
                     (fromList $ zip allWorkerIds allWorkerStates)
                     (BuildingSpace generatedBuildings generatedOccupants generatedPlantedCrops generatedBarns)
                     selectedStatus
                     generatedResources
                     (extractAnimalsFromOccupants generatedOccupants)
  return (playerData, workplaceData)

generateAnimals :: [AnimalId] -> Gen [Animal]
generateAnimals availableDogIds = do
  dogCount <- choose (0, 10)
  return $ Animal Dog <$> take dogCount availableDogIds

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
