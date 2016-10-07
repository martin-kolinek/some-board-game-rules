{-# LANGUAGE FlexibleContexts #-}

module Building where

import Util

import Data.Maybe
import Control.Monad.Except
import Data.List
import Control.Exception
import Worker
import Control.Monad.Writer
import qualified Data.Map as M
import Data.AdditiveGroup

type Position = (Int, Int)

data Building =
  Forest Position |
  Grass Position |
  Rock Position |
  Field Position |
  Cave Position |
  Passage Position |
  LivingRoom Position |
  InitialRoom Position deriving (Show, Eq)

data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight deriving (Show, Eq, Enum, Ord)

allDirections :: [Direction]
allDirections = [DirectionUp .. DirectionRight]

directionAddition :: Direction -> (Int, Int)
directionAddition DirectionUp = (0, -1)
directionAddition DirectionDown = (0, 1)
directionAddition DirectionLeft = (-1, 0)
directionAddition DirectionRight = (1, 0)

buildingPositions :: Building -> [Position]
buildingPositions (Forest pos) = [pos]
buildingPositions (Grass pos) = [pos]
buildingPositions (Rock pos) = [pos]
buildingPositions (Field pos) = [pos]
buildingPositions (InitialRoom pos) = [pos]
buildingPositions (Cave pos) = [pos]
buildingPositions (Passage pos) = [pos]
buildingPositions (LivingRoom pos) = [pos]

buildingSupportedWorkers :: Building -> Int
buildingSupportedWorkers (InitialRoom _) = 2
buildingSupportedWorkers (LivingRoom _) = 1
buildingSupportedWorkers _ = 0

newtype BuildingSpace = BuildingSpace [Building] deriving (Show, Eq)

initialBuildingSpace :: BuildingSpace
initialBuildingSpace =
  let forests = [Forest (x, y) | x <- [0..2], y <- [0..3]]
      rocks = [Rock (x, y) | x <- [3..5], y <- [0..3], (x, y) /= (3, 3), (x, y) /= (3, 2)]
      initialRoom = [InitialRoom (3, 3), Cave (3, 2)]
  in BuildingSpace (forests ++ rocks ++ initialRoom)

getBuildings :: BuildingSpace -> [Building]
getBuildings (BuildingSpace buildings) = buildings

availableBuildingPositions :: [Position]
availableBuildingPositions = getBuildings initialBuildingSpace >>= buildingPositions

getBuilding :: BuildingSpace -> Position -> Maybe Building
getBuilding (BuildingSpace buildings) position = listToMaybe [b | b <- buildings, position `elem` buildingPositions b]

isForest :: Building -> Bool
isForest (Forest _) = True
isForest _ = False

isRock :: Building -> Bool
isRock (Rock _) = True
isRock _ = False

isCave :: Building -> Bool
isCave (Cave _) = True
isCave _ = False

build :: BuildingSpace -> Building -> BuildingSpace
build (BuildingSpace buildings) building =
  let positions = buildingPositions building
      filtered = filter (null . intersect positions . buildingPositions) buildings
      newBuildings = building : filtered
      newPositions = buildingPositions =<< newBuildings
      notOverlapping = nub newPositions == newPositions
      originalPositionsOccupied = sort (buildingPositions =<< buildings) == sort newPositions
  in BuildingSpace $ assert (notOverlapping && originalPositionsOccupied) $ building : filtered

isDevelopedOutside :: Building -> Bool
isDevelopedOutside (Field _) = True
isDevelopedOutside (Grass _) = True
isDevelopedOutside (InitialRoom _) = True
isDevelopedOutside _ = False

isDevelopedInside :: Building -> Bool
isDevelopedInside (Cave _) = True
isDevelopedInside (Passage _) = True
isDevelopedInside (InitialRoom _) = True
isDevelopedInside _ = False

cutForest :: MonadError String m => Position -> Direction -> BuildingSpace -> m BuildingSpace
cutForest position direction buildingSpace =
  buildNewBuildings buildingSpace isDevelopedOutside isForest [(position, Grass), (position ^+^ directionAddition direction, Field)]

digPassage :: MonadError String m => Position -> Direction -> BuildingSpace -> m BuildingSpace
digPassage position direction buildingSpace =
  buildNewBuildings buildingSpace isDevelopedInside isRock [(position, Cave), (position ^+^ directionAddition direction, Passage)]

digCave :: MonadError String m => Position -> Direction -> BuildingSpace -> m BuildingSpace
digCave position direction buildingSpace =
  buildNewBuildings buildingSpace isDevelopedInside isRock [(position, Cave), (position ^+^ directionAddition direction, Cave)]

buildLivingRoom :: MonadError String m => Position -> BuildingSpace -> m BuildingSpace
buildLivingRoom position buildingSpace =
  buildNewBuildings buildingSpace (const True) isCave [(position, LivingRoom)]

type DevelopmentCheck = Building -> Bool
type SuitabilityCheck = Building -> Bool

buildNewBuildings :: MonadError String m => BuildingSpace -> (Building -> Bool) -> (Building -> Bool) -> [(Position, Position -> Building)] -> m BuildingSpace
buildNewBuildings buildingSpace developmentCheck suitabilityCheck newBuildings = do
  let neighbourBuildings pos = catMaybes $ getBuilding buildingSpace <$> [pos ^+^ directionAddition dir | dir <- allDirections]
      hasDevelopedNeighbours pos = any developmentCheck (neighbourBuildings pos)
  check (any (hasDevelopedNeighbours . fst) newBuildings) "Cannot reach yet"
  buildings <- forM newBuildings $ \(newPosition, buildingConstructor) -> do
    building <- checkMaybe "Invalid position" (getBuilding buildingSpace newPosition)
    check (suitabilityCheck building) "Position not suitable"
    return $ buildingConstructor newPosition
  return $ foldl' build buildingSpace buildings

data BuildingOccupant = WorkerOccupant WorkerId deriving (Eq, Show, Ord)

type BuildingOccupants = M.Map Position [BuildingOccupant]

type OccupantError = (String, Position)

areBuildingOccupantsValid :: MonadWriter [OccupantError] m => Building -> [BuildingOccupant] -> m ()
areBuildingOccupantsValid building occupants = checkWriter (length occupants <= buildingSupportedWorkers building) ("Too many people here", head $ buildingPositions building)

areOccupantsValid :: [BuildingOccupant] -> BuildingSpace -> BuildingOccupants -> [OccupantError]
areOccupantsValid allOccupants (BuildingSpace buildings) occupants = snd $ runWriter $ do
  forM_ buildings $ \building -> do
    let positions = buildingPositions building
        buildingOccupants = concat $ catMaybes $ (`M.lookup` occupants) <$> positions
    areBuildingOccupantsValid building buildingOccupants
  let positionedOccupants = concat $ M.elems occupants
  checkWriter (null $ allOccupants \\ positionedOccupants) ("Not everyone has a place", (0, 0))
  checkWriter (null $ positionedOccupants \\ allOccupants) ("There's someone who shouldn't be there", (0, 0))
  checkWriter (nub positionedOccupants == positionedOccupants) ("Occupant in multiple places", (0, 0))
  return ()

initialOccupants :: [BuildingOccupant] -> BuildingSpace -> BuildingOccupants
initialOccupants allOccupants _ = M.fromListWith mappend $ zip [(3, 3), (3, 3)] (pure <$> allOccupants)

canSupportAdditionalWorker :: [BuildingOccupant] -> BuildingSpace -> Bool
canSupportAdditionalWorker allOccupants (BuildingSpace buildings) = length allOccupants < (sum $ buildingSupportedWorkers <$> buildings)

findSpaceForWorker :: BuildingSpace -> BuildingOccupant -> BuildingOccupants -> BuildingOccupants
findSpaceForWorker (BuildingSpace buildings) newOccupant occupants = fromMaybe occupants $ do
  let buildingHasFreeSpace building = buildingSupportedWorkers building > (length $ join $ catMaybes $ (`M.lookup` occupants) <$> buildingPositions building)
  buildingToUse <- listToMaybe $ filter buildingHasFreeSpace buildings
  positionToUse <- listToMaybe $ buildingPositions buildingToUse
  return $ M.alter (Just . (newOccupant :) . fromMaybe []) positionToUse occupants
