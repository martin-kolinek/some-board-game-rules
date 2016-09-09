{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Universe where

import Player
import Workplace
import Building
import Worker
import Resources

import Data.Map
import Control.Lens

data Universe = Universe {
  _availableWorkplaces :: Map WorkplaceId WorkplaceData,
  _players :: Map PlayerId PlayerData
} deriving (Show)

makeLenses ''Universe

createWorkplaces :: Int -> Map WorkplaceId WorkplaceData
createWorkplaces count = fromList [(WorkplaceId i, updateWorkplaceAfterTurn (CutForest 0)) | i <- [0 .. count - 1]]

createWorkers :: Int -> Int -> Map WorkerId WorkerState
createWorkers initial count = fromList [(WorkerId i, initialWorkerState) | i <- [initial .. initial + count - 1]]

createPlayers :: [Int] -> Map PlayerId PlayerData
createPlayers numbersOfWorkers = fromList
  [(PlayerId i,
    PlayerData (PlayerId i) (createWorkers initial count) initialBuildingSpace empty (if i == 0 then MovingWorker else Waiting) Nothing initialResources)
      | (i, count, initial) <- zip3 [0..] numbersOfWorkers (scanl (+) 0 numbersOfWorkers)]

initialUniverse :: Universe
initialUniverse =
  let withoutOccupants = Universe (createWorkplaces 12) (createPlayers [2, 3])
      assignInitialWorkers plData = set buildingOccupants (initialOccupants (allOccupants plData) (plData ^. buildingSpace)) plData
  in over (players . traverse) assignInitialWorkers withoutOccupants
