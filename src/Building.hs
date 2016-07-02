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

initialBuildingSpace = BuildingSpace [Forest (0, 0), Forest (1, 0), Forest (0, 1), Forest (1, 1), Rock (2, 0), Rock (3, 0), InitialRoom (3, 1), InitialRoom (2, 1)]

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
initialOccupants allOccupants (BuildingSpace buildings) = M.fromListWith mappend $ zip [(3, 1), (3, 1), (2, 1), (2, 1)] (pure <$> allOccupants)
