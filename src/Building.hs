{-# LANGUAGE FlexibleContexts #-}

module Building where

import Util

import Data.Maybe
import Control.Monad.Except
import Data.List
import Control.Exception

type Position = (Int, Int)

data Building =
  Forest Position |
  Grass Position |
  Rock Position |
  InitialRoom Position

buildingPositions (Forest pos) = [pos]
buildingPositions (Grass pos) = [pos]
buildingPositions (Rock pos) = [pos]
buildingPositions (InitialRoom pos) = [pos]

newtype BuildingSpace = BuildingSpace [Building]

initialBuildingSpace = [Forest (0, 0), Forest (1, 0), Forest (0, 1), Forest (1, 1), Rock (2, 0), Rock (3, 0), Rock (3, 1), InitialRoom (2, 1)]

getBuildings (BuildingSpace buildings) = buildings

getBuilding (BuildingSpace buildings) position = listToMaybe [b | b <- buildings, position `elem` buildingPositions b]

isForest (Forest _) = True
isForest _ = False

build :: Building -> BuildingSpace -> BuildingSpace
build building (BuildingSpace buildings) =
  let positions = buildingPositions building
      filtered = filter (null . intersect positions . buildingPositions) buildings
      newBuildings = building : filtered
      newPositions = buildingPositions =<< newBuildings
      notOverlapping = nub newPositions == newPositions
      originalPositionsOccupied = (buildingPositions =<< buildings) == newPositions
  in BuildingSpace $ assert (notOverlapping && originalPositionsOccupied) $ building : filtered

cutForest :: MonadError String m => Position -> BuildingSpace -> m BuildingSpace
cutForest position buildingSpace = do
  building <- checkMaybe (getBuilding buildingSpace position) "Invalid position"
  check (isForest building) "Cutting forest not in a forest"
  return $ build (Grass position) buildingSpace
