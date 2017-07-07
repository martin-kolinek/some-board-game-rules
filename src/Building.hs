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
import Data.Either (isRight)

type Position = (Int, Int)

data SmallBuildingType =
  Forest |
  Grass |
  Rock |
  Field |
  Cave |
  Passage |
  LivingRoom |
  InitialRoom |
  SmallPasture deriving (Show, Eq, Ord, Enum)

data LargeBuildingType = LargePasture deriving (Show, Eq, Ord, Enum)

data Building = SmallBuilding SmallBuildingType Position | LargeBuilding LargeBuildingType Position Direction deriving (Show, Eq)

data BuildingDescription =
  SingleSmallBuildingDesc SmallBuildingType |
  DoubleSmallBuildingDesc SmallBuildingType SmallBuildingType |
  LargeBuildingDesc LargeBuildingType
  deriving (Show, Eq, Ord)


data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight deriving (Show, Eq, Enum, Ord)

data BuildingOccupant = WorkerOccupant WorkerId | AnimalOccupant Animal deriving (Eq, Show, Ord)

type BuildingOccupants = M.Map Position [BuildingOccupant]

type OccupantError = (String, Position)

data CropType = Potatoes | Wheat deriving (Eq, Show, Ord)

data PlantedCrop = PlantedCrop CropType Int deriving (Eq, Show)

data BuildingSpace = BuildingSpace {
  _buildingSpaceBuildings :: [Building],
  _buildingSpaceOccupants :: BuildingOccupants,
  _buildingSpaceCrops :: M.Map Position PlantedCrop,
  _buildingSpaceBarns :: [Position]
} deriving (Show, Eq)

allDirections :: [Direction]
allDirections = [DirectionUp .. DirectionRight]

directionAddition :: Direction -> (Int, Int)
directionAddition DirectionUp = (0, -1)
directionAddition DirectionDown = (0, 1)
directionAddition DirectionLeft = (-1, 0)
directionAddition DirectionRight = (1, 0)

buildingPositions :: Building -> [Position]
buildingPositions (SmallBuilding _ pos) = [pos]
buildingPositions (LargeBuilding _ pos dir) = [pos, pos ^+^ directionAddition dir]

buildingSupportedWorkers :: Building -> Int
buildingSupportedWorkers (SmallBuilding InitialRoom _) = 2
buildingSupportedWorkers (SmallBuilding LivingRoom _) = 1
buildingSupportedWorkers _ = 0

buildingSupportedAnimals :: Building -> Bool -> Int
buildingSupportedAnimals (SmallBuilding InitialRoom _) _ = 2
buildingSupportedAnimals (SmallBuilding SmallPasture _) hasBarn = 2 * (if hasBarn then 2 else 1)
buildingSupportedAnimals (LargeBuilding LargePasture _ _) hasBarn = 4 * (if hasBarn then 2 else 1)
buildingSupportedAnimals _ _ = 0

makeLenses ''BuildingSpace

initialBuildingSpace :: [BuildingOccupant] -> BuildingSpace
initialBuildingSpace workers =
  let forests = [SmallBuilding Forest (x, y) | x <- [0..2], y <- [0..3]]
      rocks = [SmallBuilding Rock (x, y) | x <- [3..5], y <- [0..3], (x, y) /= (3, 3), (x, y) /= (3, 2)]
      initialRoom = [SmallBuilding InitialRoom (3, 3), SmallBuilding Cave (3, 2)]
      buildings = (forests ++ rocks ++ initialRoom)
  in BuildingSpace buildings (initialOccupants workers buildings) M.empty []

getBuildings :: BuildingSpace -> [Building]
getBuildings = view buildingSpaceBuildings

availableBuildingPositions :: [Position]
availableBuildingPositions = getBuildings (initialBuildingSpace []) >>= buildingPositions

getBuilding :: [Building] -> Position -> Maybe Building
getBuilding buildings position = listToMaybe [b | b <- buildings, position `elem` buildingPositions b]

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
isDevelopedOutside (LargeBuilding LargePasture _ _) = True
isDevelopedOutside (SmallBuilding buildingType _) = buildingType `elem` [Field, Grass, InitialRoom, SmallPasture]

isDevelopedInside :: Building -> Bool
isDevelopedInside (SmallBuilding buildingType _) = buildingType `elem` [Cave, Passage, InitialRoom, LivingRoom]
isDevelopedInside (LargeBuilding _ _ _) = False

isDevelopedFor :: Building -> Building -> Bool
isDevelopedFor (SmallBuilding Cave _) = isDevelopedInside
isDevelopedFor (SmallBuilding Passage _) = isDevelopedInside
isDevelopedFor (SmallBuilding LivingRoom _) = const True
isDevelopedFor (SmallBuilding Field _) = isDevelopedOutside
isDevelopedFor (SmallBuilding Grass _) = isDevelopedOutside
isDevelopedFor (SmallBuilding Forest _) = const False
isDevelopedFor (SmallBuilding Rock _) = const False
isDevelopedFor (SmallBuilding InitialRoom _) = const False
isDevelopedFor (SmallBuilding SmallPasture _) = const True
isDevelopedFor (LargeBuilding LargePasture _ _) = isDevelopedOutside

isUnderlyingBuilding :: Building -> Building -> Bool
isUnderlyingBuilding (SmallBuilding Cave _) (SmallBuilding Rock _) = True
isUnderlyingBuilding (SmallBuilding Passage _) (SmallBuilding Rock _) = True
isUnderlyingBuilding (SmallBuilding LivingRoom _) (SmallBuilding Cave _) = True
isUnderlyingBuilding (SmallBuilding Field _) (SmallBuilding Forest _) = True
isUnderlyingBuilding (SmallBuilding Grass _) (SmallBuilding Forest _) = True
isUnderlyingBuilding (SmallBuilding Forest _) (SmallBuilding Forest _) = True
isUnderlyingBuilding (SmallBuilding Rock _) (SmallBuilding Rock _) = True
isUnderlyingBuilding (SmallBuilding InitialRoom _) (SmallBuilding InitialRoom _) = True
isUnderlyingBuilding (SmallBuilding SmallPasture _) (SmallBuilding Grass _) = True
isUnderlyingBuilding (LargeBuilding LargePasture _ _) (SmallBuilding Grass _) = True
isUnderlyingBuilding _ _ = False

canHaveBarn :: Building -> Bool
canHaveBarn (SmallBuilding Forest _) = True
canHaveBarn (SmallBuilding Grass _) = True
canHaveBarn (SmallBuilding SmallPasture _) = True
canHaveBarn (LargeBuilding LargePasture _ _) = True
canHaveBarn _ = False

canBuildNewBuildings :: BuildingDescription -> BuildingSpace -> Bool
canBuildNewBuildings buildingDescription buildingSpace = any canBuild possibilities
  where canBuild (pos, dir) = isRight $ buildNewBuildings pos dir buildingDescription buildingSpace
        possibilities = [((x, y), dir) | x <- [0..5], y <- [0..3], dir <- allDirections]

buildNewBuildings ::
  MonadError String f =>
  Position
  -> Direction
  -> BuildingDescription
  -> BuildingSpace
  -> f BuildingSpace
buildNewBuildings position direction buildingDescription buildingSpace =
  traverseOf buildingSpaceBuildings (buildNewBuildingsInBuildingCollection barns newBuildings) buildingSpace
  where barns = buildingSpace ^. buildingSpaceBarns
        newBuildings = case buildingDescription of
          (SingleSmallBuildingDesc buildingType) -> [SmallBuilding buildingType position]
          (DoubleSmallBuildingDesc buildingType1 buildingType2) -> [SmallBuilding buildingType1 position, SmallBuilding buildingType2 (position ^+^ directionAddition direction)]
          (LargeBuildingDesc buildingType) -> [LargeBuilding buildingType position direction]

buildNewBuildingsInBuildingCollection :: MonadError String m => [Position] -> [Building] -> [Building] -> m [Building]
buildNewBuildingsInBuildingCollection barns newBuildings originalBuildings = do
  let neighbourBuildings pos = catMaybes $ getBuilding originalBuildings <$> [pos ^+^ directionAddition dir | dir <- allDirections]
      developmentCheck :: Building -> Bool
      developmentCheck = any id . sequence (isDevelopedFor <$> newBuildings)
      hasDevelopedNeighbours pos = any developmentCheck (neighbourBuildings pos)
  check "Cannot reach yet" (any hasDevelopedNeighbours (newBuildings >>= buildingPositions))
  buildings <- forM newBuildings $ \newBuilding -> do
    let positions = buildingPositions newBuilding
        hasBarn = not $ null $ positions `intersect` barns
        underlyingBuildingMaybes = getBuilding originalBuildings <$> positions
    check "Cannot build under barn" $ (not hasBarn) || canHaveBarn newBuilding
    underlyingBuildings <- checkMaybe "Invalid position" (sequence underlyingBuildingMaybes)
    check "Position not suitable" $ all (isUnderlyingBuilding newBuilding) underlyingBuildings
    return newBuilding
  return $ foldl' build originalBuildings buildings


isWorkerOccupant :: BuildingOccupant -> Bool
isWorkerOccupant (WorkerOccupant _) = True
isWorkerOccupant _ = False

isDogOccupant :: BuildingOccupant -> Bool
isDogOccupant (AnimalOccupant (Animal Dog _)) = True
isDogOccupant _ = False

areBuildingOccupantsValid :: MonadWriter [OccupantError] m => Building -> [BuildingOccupant] -> Bool -> m ()
areBuildingOccupantsValid building occupants hasBarn = do
  checkWriter (length workerOccupants <= buildingSupportedWorkers building) ("Too many people here", head $ buildingPositions building)
  checkWriter (length animalOccupantGroups <= 1) ("Too many types of animals here", head $ buildingPositions building)
  let (SmallBuilding buildingType pos) = building
  forM_ animalOccupantGroups $ \(animalType, groupAnimals) -> case animalType of
    Sheep -> do
      let sheepAmount = length groupAnimals
          sheepFit = buildingSupportedAnimals building hasBarn >= sheepAmount
          sheepGuarded = buildingType `elem` [Grass, SmallPasture] && dogAmount >= sheepAmount - 1
      checkWriter (sheepFit || sheepGuarded) ("Too many unguarded sheep here", pos)
    Cow -> checkWriter (buildingSupportedAnimals building hasBarn >= length groupAnimals) ("Too many cows here", pos)
  where workerOccupants = filter isWorkerOccupant occupants
        farmAnimalGroup (AnimalOccupant (Animal (FarmAnimalType animalType) _)) = Just animalType
        farmAnimalGroup _ = Nothing
        dogAmount = length $ filter isDogOccupant occupants
        animalOccupantGroups = groupSort $ catMaybes $ (\occ -> (,occ) <$> (farmAnimalGroup occ)) <$> occupants

areOccupantsValid :: [BuildingOccupant] -> BuildingSpace -> [OccupantError]
areOccupantsValid allOccupants (BuildingSpace buildings occupants _ barns) = snd $ runWriter $ do
  forM_ buildings $ \building -> do
    let positions = buildingPositions building
        buildingOccupants = concat $ catMaybes $ (`M.lookup` occupants) <$> positions
        hasBarn = not $ null $ positions `intersect` barns
    areBuildingOccupantsValid building buildingOccupants hasBarn
  let positionedOccupants = concat $ M.elems occupants
  checkWriter (null $ allOccupants \\ positionedOccupants) ("Not everyone has a place", (0, 0))
  checkWriter (null $ positionedOccupants \\ allOccupants) ("There's someone who shouldn't be there", (0, 0))
  checkWriter (nub positionedOccupants == positionedOccupants) ("Occupant in multiple places", (0, 0))
  return ()

initialOccupants :: [BuildingOccupant] -> [Building] -> BuildingOccupants
initialOccupants allOccupants _ = M.fromListWith mappend $ zip [(3, 3), (3, 3)] (pure <$> allOccupants)

canSupportAdditionalWorker :: [BuildingOccupant] -> BuildingSpace -> Bool
canSupportAdditionalWorker allOccupants (BuildingSpace buildings _ _ _) = length workerOccupants < (sum $ buildingSupportedWorkers <$> buildings)
  where workerOccupants = filter isWorkerOccupant allOccupants

findSpaceForWorker :: BuildingOccupant -> BuildingSpace -> BuildingSpace
findSpaceForWorker newOccupant buildSpace@(BuildingSpace buildings occupants _ _) = fromMaybe buildSpace $ do
  let buildingHasFreeSpace building = buildingSupportedWorkers building >
        (length $ filter isWorkerOccupant $ join $ catMaybes $ (`M.lookup` occupants) <$> buildingPositions building)
  buildingToUse <- listToMaybe $ filter buildingHasFreeSpace buildings
  positionToUse <- listToMaybe $ buildingPositions buildingToUse
  return $ buildSpace & buildingSpaceOccupants .~ M.alter (Just . (newOccupant :) . fromMaybe []) positionToUse occupants

plantCropsInBuildingSpace :: MonadError String m => [(CropType, Position)] -> BuildingSpace -> m BuildingSpace
plantCropsInBuildingSpace crops buildingSpace = foldM (flip $ uncurry plantCropInBuildingSpace) buildingSpace crops

plantCropInBuildingSpace :: MonadError [Char] m => CropType -> Position -> BuildingSpace -> m BuildingSpace
plantCropInBuildingSpace cropType position buildingSpace = do
  check "Must plant on field" (SmallBuilding Field position `elem` buildingSpace ^. buildingSpaceBuildings)
  check "Cannot plant over other crops" (M.notMember position $ buildingSpace ^. buildingSpaceCrops)
  let plantCropType Wheat = PlantedCrop Wheat 3
      plantCropType Potatoes = PlantedCrop Potatoes 2
  return $ over buildingSpaceCrops (M.insert position (plantCropType cropType)) buildingSpace

cropResource :: CropType -> Lens' Resources Int
cropResource Potatoes = potatoAmount
cropResource Wheat = wheatAmount

buildingCost :: SmallBuildingType -> Resources
buildingCost LivingRoom = wood 4 ^+^ stone 3
buildingCost _ = zeroV

canBuildBarn :: BuildingSpace -> Bool
canBuildBarn (BuildingSpace buildings _ _ barns) = any hasNoBarn availableBarnPositions
  where hasNoBarn = not . (`elem` barns)
        availableBarnPositions = [pos | building <- buildings, pos <- buildingPositions building, canHaveBarn building]
