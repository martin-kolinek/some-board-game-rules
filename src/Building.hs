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
import Control.Lens
import Data.AdditiveGroup

type Position = (Int, Int)

data Building =
  Forest Position |
  Grass Position |
  Rock Position |
  InitialRoom Position deriving (Show, Eq)

data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight deriving (Show, Eq)

directionAddition DirectionUp = (0, -1)
directionAddition DirectionDown = (0, 1)
directionAddition DirectionLeft = (-1, 0)
directionAddition DirectionRight = (1, 0)

buildingPositions (Forest pos) = [pos]
buildingPositions (Grass pos) = [pos]
buildingPositions (Rock pos) = [pos]
buildingPositions (InitialRoom pos) = [pos]

newtype BuildingSpace = BuildingSpace [Building] deriving (Show, Eq)

initialBuildingSpace =
  let forests = [Forest (x, y) | x <- [0..3], y <- [0..3]]
      rocks = [Rock (x, y) | x <- [4..7], y <- [0..3], (x, y) /= (4, 3), (x, y) /= (4, 2)]
      initialRoom = [InitialRoom (4, 3), InitialRoom (4, 2)]
  in BuildingSpace (forests ++ rocks ++ initialRoom)

getBuildings (BuildingSpace buildings) = buildings

availableBuildingPositions :: [Position]
availableBuildingPositions = getBuildings initialBuildingSpace >>= buildingPositions

getBuilding (BuildingSpace buildings) position = listToMaybe [b | b <- buildings, position `elem` buildingPositions b]

isForest (Forest _) = True
isForest _ = False

build :: BuildingSpace -> Building -> BuildingSpace
build (BuildingSpace buildings) building =
  let positions = buildingPositions building
      filtered = filter (null . intersect positions . buildingPositions) buildings
      newBuildings = building : filtered
      newPositions = buildingPositions =<< newBuildings
      notOverlapping = nub newPositions == newPositions
      originalPositionsOccupied = (buildingPositions =<< buildings) == newPositions
  in BuildingSpace $ assert (notOverlapping && originalPositionsOccupied) $ building : filtered

cutForest :: MonadError String m => Position -> Direction -> BuildingSpace -> m BuildingSpace
cutForest position direction buildingSpace = do
  let newPositions = [position, position ^+^ directionAddition direction]
  buildings <- forM newPositions $ \newPosition -> do
    building <- checkMaybe "Invalid position" (getBuilding buildingSpace newPosition)
    check (isForest building) "Cutting forest not in a forest"
    return $ Grass newPosition
  return $ foldl' build buildingSpace buildings

data BuildingOccupant = WorkerOccupant WorkerId deriving (Eq, Show, Ord)

type BuildingOccupants = M.Map Position [BuildingOccupant]

type OccupantError = (String, Position)

areBuildingOccupantsValid :: MonadWriter [OccupantError] m => Building -> [BuildingOccupant] -> m ()
areBuildingOccupantsValid (InitialRoom pos) occupants = checkWriter (length occupants <= 2) ("Too many people", pos)
areBuildingOccupantsValid building occupants = checkWriter (null occupants) ("There can't be anyone here", head $ buildingPositions building)

areOccupantsValid :: [BuildingOccupant] -> BuildingSpace -> BuildingOccupants -> [OccupantError]
areOccupantsValid allOccupants (BuildingSpace buildings) occupants = snd $ runWriter $ do
  forM_ buildings $ \building -> do
    let positions = buildingPositions building
        buildingOccupants = concat $ catMaybes $ (`M.lookup` occupants) <$> positions
    areBuildingOccupantsValid building buildingOccupants
  let positionedOccupants = concat $ M.elems occupants
  checkWriter (null $ positionedOccupants \\ allOccupants) ("Not everyone has a place", (0, 0))
  return ()

initialOccupants :: [BuildingOccupant] -> BuildingSpace -> BuildingOccupants
initialOccupants allOccupants (BuildingSpace buildings) = M.fromListWith mappend $ zip [(4, 3), (4, 3), (4, 2), (4, 2)] (pure <$> allOccupants)
