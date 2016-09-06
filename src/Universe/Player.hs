module Universe.Player where

import Control.Lens hiding (universe)
import Data.Maybe
import Data.Map hiding (filter)

import Player
import Universe
import Worker
import Resources

getCurrentPlayer :: Universe -> Maybe PlayerId
getCurrentPlayer universe =
  listToMaybe [view playerId player | player <- toListOf (players . traverse) universe, view playerStatus player /= Waiting]

getPlayers :: Universe -> [PlayerId]
getPlayers = toListOf (players . folding keys)

getPlayerStatus :: Universe -> PlayerId -> PlayerStatus
getPlayerStatus universe player = fromMaybe Waiting $ universe ^? (players . ix player . playerStatus)

getPlayerResources :: Universe -> PlayerId -> Resources
getPlayerResources universe player = fromMaybe initialResources $ universe ^? (players . ix player . playerResources)

currentPlayerData :: Traversal' Universe PlayerData
currentPlayerData fres universe = (players . fromMaybe ignored (ix <$> getCurrentPlayer universe)) fres universe

nextPlayer :: Universe -> Maybe PlayerId
nextPlayer universe = do
  currentPlayer <- getCurrentPlayer universe
  let playerIds = keys (universe ^. players)
      hasFreeWorkers player = has (players . ix player . workers . folding elems . currentWorkplace . filtered isNothing) universe
      candidatePlayers = (tail . dropWhile (/= currentPlayer)) $ playerIds ++ playerIds
  listToMaybe $ filter hasFreeWorkers candidatePlayers
