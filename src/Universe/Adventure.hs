module Universe.Adventure where

import Player
import Resources

import Control.Lens

data AdventureReward = WoodReward

applyReward :: AdventureReward -> Player.PlayerData -> Player.PlayerData
applyReward WoodReward playerData = playerData & (playerResources . woodAmount) +~ 1
