{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Building where

import Util
import Resources

import Data.Maybe
import Control.Monad
import Control.Monad.Except
import Data.List
import Control.Exception
import Worker
import Control.Monad.Writer
import qualified Data.Map as M
import Data.AdditiveGroup
import Control.Lens
import Data.List.Extra

type Position = (Int, Int)

data BuildingType =
  Forest |
  Grass |
  Rock |
  Field |
  Cave |
  Passage |
  LivingRoom |
  InitialRoom |
  SmallPasture deriving (Show, Eq, Ord, Enum)

data Building = Building BuildingType Position deriving (Show, Eq)

data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight deriving (Show, Eq, Enum, Ord)

data BuildingOccupant = WorkerOccupant WorkerId | AnimalOccupant Animal deriving (Eq, Show, Ord)

type BuildingOccupants = M.Map Position [BuildingOccupant]

type OccupantError = (String, Position)

data CropType = Potatoes | Wheat deriving (Eq, Show, Ord)

data PlantedCrop = PlantedCrop CropType Int deriving (Eq, Show)

data BuildingSpace = BuildingSpace {
  _buildingSpaceBuildings :: [Building],
  _buildingSpaceOccupants :: BuildingOccupants,
  _buildingSpaceCrops :: M.Map Position PlantedCrop
} deriving (Show, Eq)

allDirections :: [Direction]
allDirections = [DirectionUp .. DirectionRight]

directionAddition :: Direction -> (Int, Int)
directionAddition DirectionUp = (0, -1)
directionAddition DirectionDown = (0, 1)
directionAddition DirectionLeft = (-1, 0)
directionAddition DirectionRight = (1, 0)

buildingPositions :: Building -> [Position]
buildingPositions (Building _ pos) = [pos]

buildingSupportedWorkers :: Building -> Int
buildingSupportedWorkers (Building InitialRoom _) = 2
buildingSupportedWorkers (Building LivingRoom _) = 1
buildingSupportedWorkers _ = 0

buildingSupportedAnimals :: Building -> Int
buildingSupportedAnimals (Building InitialRoom _) = 2
buildingSupportedAnimals (Building SmallPasture _) = 2
buildingSupportedAnimals _ = 0

makeLenses ''BuildingSpace

initialBuildingSpace :: [BuildingOccupant] -> BuildingSpace
initialBuildingSpace workers =
  let forests = [Building Forest (x, y) | x <- [0..2], y <- [0..3]]
      rocks = [Building Rock (x, y) | x <- [3..5], y <- [0..3], (x, y) /= (3, 3), (x, y) /= (3, 2)]
      initialRoom = [Building InitialRoom (3, 3), Building Cave (3, 2)]
      buildings = (forests ++ rocks ++ initialRoom)
  in BuildingSpace buildings (initialOccupants workers buildings) M.empty

getBuildings :: BuildingSpace -> [Building]
getBuildings = view buildingSpaceBuildings

availableBuildingPositions :: [Position]
availableBuildingPositions = getBuildings (initialBuildingSpace []) >>= buildingPositions

getBuilding :: [Building] -> Position -> Maybe Building
getBuilding buildings position = listToMaybe [b | b <- buildings, position `elem` buildingPositions b]

getBuildingType :: Building -> BuildingType
getBuildingType (Building tp _) = tp

build :: [Building] -> Building -> [Building]
build buildings building =
  let positions = buildingPositions building
      filteredBuildings = filter (null . intersect positions . buildingPositions) buildings
      newBuildings = building : filteredBuildings
      newPositions = buildingPositions =<< newBuildings
      notOverlapping = nub newPositions == newPositions
      originalPositionsOccupied = sort (buildingPositions =<< buildings) == sort newPositions
  in assert (notOverlapping && originalPositionsOccupied) $ newBuildings

isDevelopedOutside :: Building -> Bool
isDevelopedOutside building = getBuildingType building `elem` [Field, Grass, InitialRoom, SmallPasture]

isDevelopedInside :: Building -> Bool
isDevelopedInside building = getBuildingType building `elem` [Cave, Passage, InitialRoom, LivingRoom]

isDevelopedFor :: BuildingType -> Building -> Bool
isDevelopedFor Cave = isDevelopedInside
isDevelopedFor Passage = isDevelopedInside
isDevelopedFor LivingRoom = const True
isDevelopedFor Field = isDevelopedOutside
isDevelopedFor Grass = isDevelopedOutside
isDevelopedFor Forest = const False
isDevelopedFor Rock = const False
isDevelopedFor InitialRoom = const False
isDevelopedFor SmallPasture = const True

getUnderlyingBuilding :: BuildingType -> BuildingType
getUnderlyingBuilding Cave = Rock
getUnderlyingBuilding Passage = Rock
getUnderlyingBuilding LivingRoom = Cave
getUnderlyingBuilding Field = Forest
getUnderlyingBuilding Grass = Forest
getUnderlyingBuilding Forest = Forest
getUnderlyingBuilding Rock = Rock
getUnderlyingBuilding InitialRoom = InitialRoom
getUnderlyingBuilding SmallPasture = Grass

type DevelopmentCheck = Building -> Bool
type UnderlyingBuilding = BuildingType

buildNewBuildings ::
  MonadError String f =>
  Position
  -> Direction
  -> [BuildingType]
  -> BuildingSpace
  -> f BuildingSpace
buildNewBuildings position direction buildingTypes =
  traverseOf buildingSpaceBuildings (buildNewBuildingsInBuildingCollection newBuildings)
  where newBuildings = zip [position, position ^+^ directionAddition direction] buildingTypes

buildNewBuildingsInBuildingCollection :: MonadError String m => [(Position, BuildingType)] -> [Building] -> m [Building]
buildNewBuildingsInBuildingCollection newBuildings originalBuildings = do
  let neighbourBuildings pos = catMaybes $ getBuilding originalBuildings <$> [pos ^+^ directionAddition dir | dir <- allDirections]
      developmentCheck :: Building -> Bool
      developmentCheck = any id . sequence (isDevelopedFor <$> snd <$> newBuildings)
      hasDevelopedNeighbours pos = any developmentCheck (neighbourBuildings pos)
  check "Cannot reach yet" (any (hasDevelopedNeighbours . fst) newBuildings)
  buildings <- forM newBuildings $ \(newPosition, buildingType) -> do
    building <- checkMaybe "Invalid position" (getBuilding originalBuildings newPosition)
    let underlyingBuilding = getUnderlyingBuilding buildingType
    check "Position not suitable" (getBuildingType building == underlyingBuilding)
    return $ Building buildingType newPosition
  return $ foldl' build originalBuildings buildings


isWorkerOccupant :: BuildingOccupant -> Bool
isWorkerOccupant (WorkerOccupant _) = True
isWorkerOccupant _ = False

isDogOccupant :: BuildingOccupant -> Bool
isDogOccupant (AnimalOccupant (Animal Dog _)) = True
isDogOccupant _ = False

areBuildingOccupantsValid :: MonadWriter [OccupantError] m => Building -> [BuildingOccupant] -> m ()
areBuildingOccupantsValid building occupants = do
  checkWriter (length workerOccupants <= buildingSupportedWorkers building) ("Too many people here", head $ buildingPositions building)
  checkWriter (length animalOccupantGroups <= 1) ("Too many types of animals here", head $ buildingPositions building)
  let (Building buildingType pos) = building
  forM_ animalOccupantGroups $ \(animalType, groupAnimals) -> case animalType of
    Sheep -> do
      let sheepAmount = length groupAnimals
          sheepFit = buildingSupportedAnimals building >= sheepAmount
          sheepGuarded = buildingType `elem` [Grass, SmallPasture] && dogAmount >= sheepAmount - 1
      checkWriter (sheepFit || sheepGuarded) ("Too many unguarded sheep here", pos)
    Cow -> checkWriter (buildingSupportedAnimals building >= length groupAnimals) ("Too many cows here", pos)
  where workerOccupants = filter isWorkerOccupant occupants
        farmAnimalGroup (AnimalOccupant (Animal (FarmAnimalType animalType) _)) = Just animalType
        farmAnimalGroup _ = Nothing
        dogAmount = length $ filter isDogOccupant occupants
        animalOccupantGroups = groupSort $ catMaybes $ (\occ -> (,occ) <$> (farmAnimalGroup occ)) <$> occupants

areOccupantsValid :: [BuildingOccupant] -> BuildingSpace -> [OccupantError]
areOccupantsValid allOccupants (BuildingSpace buildings occupants _) = snd $ runWriter $ do
  forM_ buildings $ \building -> do
    let positions = buildingPositions building
        buildingOccupants = concat $ catMaybes $ (`M.lookup` occupants) <$> positions
    areBuildingOccupantsValid building buildingOccupants
  let positionedOccupants = concat $ M.elems occupants
  checkWriter (null $ allOccupants \\ positionedOccupants) ("Not everyone has a place", (0, 0))
  checkWriter (null $ positionedOccupants \\ allOccupants) ("There's someone who shouldn't be there", (0, 0))
  checkWriter (nub positionedOccupants == positionedOccupants) ("Occupant in multiple places", (0, 0))
  return ()

initialOccupants :: [BuildingOccupant] -> [Building] -> BuildingOccupants
initialOccupants allOccupants _ = M.fromListWith mappend $ zip [(3, 3), (3, 3)] (pure <$> allOccupants)

canSupportAdditionalWorker :: [BuildingOccupant] -> BuildingSpace -> Bool
canSupportAdditionalWorker allOccupants (BuildingSpace buildings _ _) = length workerOccupants < (sum $ buildingSupportedWorkers <$> buildings)
  where workerOccupants = filter isWorkerOccupant allOccupants

findSpaceForWorker :: BuildingOccupant -> BuildingSpace -> BuildingSpace
findSpaceForWorker newOccupant buildSpace@(BuildingSpace buildings occupants _) = fromMaybe buildSpace $ do
  let buildingHasFreeSpace building = buildingSupportedWorkers building >
        (length $ filter isWorkerOccupant $ join $ catMaybes $ (`M.lookup` occupants) <$> buildingPositions building)
  buildingToUse <- listToMaybe $ filter buildingHasFreeSpace buildings
  positionToUse <- listToMaybe $ buildingPositions buildingToUse
  return $ buildSpace & buildingSpaceOccupants .~ M.alter (Just . (newOccupant :) . fromMaybe []) positionToUse occupants

plantCropsInBuildingSpace :: MonadError String m => [(CropType, Position)] -> BuildingSpace -> m BuildingSpace
plantCropsInBuildingSpace crops buildingSpace = foldM (flip $ uncurry plantCropInBuildingSpace) buildingSpace crops

plantCropInBuildingSpace :: MonadError [Char] m => CropType -> Position -> BuildingSpace -> m BuildingSpace
plantCropInBuildingSpace cropType position buildingSpace = do
  check "Must plant on field" (Building Field position `elem` buildingSpace ^. buildingSpaceBuildings)
  check "Cannot plant over other crops" (M.notMember position $ buildingSpace ^. buildingSpaceCrops)
  let plantCropType Wheat = PlantedCrop Wheat 3
      plantCropType Potatoes = PlantedCrop Potatoes 2
  return $ over buildingSpaceCrops (M.insert position (plantCropType cropType)) buildingSpace

cropResource :: CropType -> Lens' Resources Int
cropResource Potatoes = potatoAmount
cropResource Wheat = wheatAmount

buildingCost :: BuildingType -> Resources
buildingCost LivingRoom = wood 4 ^+^ stone 3
buildingCost _ = zeroV
