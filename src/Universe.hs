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
  _availableWorkplaces :: Map WorkplaceId WorkplaceAction,
  _players :: Map PlayerId PlayerData
} deriving (Show)

makeLenses ''Universe

createWorkplaces count increaseScoreCount = fromList [(WorkplaceId i, if i < increaseScoreCount then IncreaseScore else CutForest) | i <- [0 .. count - 1]]

createWorkers initial count = fromList [(WorkerId i, initialWorkerState) | i <- [initial .. initial + count - 1]]

createPlayers numbersOfWorkers = fromList
  [(PlayerId i,
    PlayerData (PlayerId i) (createWorkers initial count) initialBuildingSpace empty 0 (if i == 0 then MovingWorker else Waiting) Nothing initialResources)
      | (i, count, initial) <- zip3 [0..] numbersOfWorkers (scanl (+) 0 numbersOfWorkers)]

initialUniverse =
  let withoutOccupants = Universe (createWorkplaces 12 6) (createPlayers [2, 3])
      assignInitialWorkers plData = set buildingOccupants (initialOccupants (allOccupants plData) (plData ^. buildingSpace)) plData
  in over (players . traverse) assignInitialWorkers withoutOccupants
