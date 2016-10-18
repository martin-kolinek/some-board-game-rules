{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Universe where

import Player
import Workplace

import Data.Map
import Control.Lens

data Universe = Universe {
  _availableWorkplaces :: Map WorkplaceId WorkplaceData,
  _players :: Map PlayerId PlayerData,
  _startingPlayer :: PlayerId
} deriving (Show)

makeLenses ''Universe

initialUniverse :: Universe
initialUniverse = Universe initialWorkplaces initialPlayers (head $ keys initialPlayers)
