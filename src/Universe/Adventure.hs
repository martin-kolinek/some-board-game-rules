module Universe.Adventure where

import Player
import Resources
import Actions
import Building

import Control.Lens

data AdventureReward = WoodReward | GrassReward

applyReward :: AdventureReward -> Player.PlayerData -> Player.PlayerData
applyReward WoodReward playerData = playerData & (playerResources . woodAmount) +~ 1
applyReward GrassReward playerData = playerData

rewardInteraction :: AdventureReward -> Maybe ActionInteraction
rewardInteraction GrassReward = Just $ BuildBuildingsInteraction [Grass]
rewardInteraction _ = Nothing
