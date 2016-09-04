module Universe.Player where

import Control.Lens
import Data.Maybe
import Data.Map hiding (filter)

import Player
import Universe
import Worker
import Workplace
import Universe.Building
import Universe.Workplace

getCurrentPlayer :: Universe -> Maybe PlayerId
getCurrentPlayer universe =
  listToMaybe [view playerId player | player <- toListOf (players . traverse) universe, view playerStatus player /= Waiting]

getPlayers :: Universe -> [PlayerId]
getPlayers = toListOf (players . folding keys)

getScore :: Universe -> PlayerId -> Int
getScore universe playerId = fromMaybe 0 $ universe ^? (players . ix playerId . score)

getPlayerStatus :: Universe -> PlayerId -> PlayerStatus
getPlayerStatus universe playerId = fromMaybe Waiting $ universe ^? (players . ix playerId . playerStatus)

currentPlayerData :: Traversal' Universe PlayerData
currentPlayerData fres universe = (players . fromMaybe ignored (ix <$> getCurrentPlayer universe)) fres universe

nextPlayer :: Universe -> Maybe PlayerId
nextPlayer universe = do
  currentPlayer <- getCurrentPlayer universe
  let playerIds = keys (universe ^. players)
      hasFreeWorkers playerId = has (players . ix playerId . workers . folding elems . currentWorkplace . filtered isNothing) universe
      candidatePlayers = (tail . dropWhile (/= currentPlayer)) $ playerIds ++ playerIds
  listToMaybe $ filter hasFreeWorkers candidatePlayers

