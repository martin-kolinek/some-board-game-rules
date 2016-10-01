{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Universe where

import Player
import Workplace
import Building

import Data.Map
import Control.Lens

data Universe = Universe {
  _availableWorkplaces :: Map WorkplaceId WorkplaceData,
  _players :: Map PlayerId PlayerData
} deriving (Show)

makeLenses ''Universe

initialUniverse :: Universe
initialUniverse =
  let withoutOccupants = Universe initialWorkplaces initialPlayers
      assignInitialWorkers plData = set buildingOccupants (initialOccupants (allOccupants plData) (plData ^. buildingSpace)) plData
  in over (players . traverse) assignInitialWorkers withoutOccupants
