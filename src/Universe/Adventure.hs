module Universe.Adventure where

import Player
import Resources
import Actions
import Building

import Control.Lens

data AdventureReward = WoodReward | GrassReward | SmallPastureReward

applyReward :: AdventureReward -> Player.PlayerData -> Player.PlayerData
applyReward WoodReward playerData = playerData & (playerResources . woodAmount) +~ 1
applyReward _ playerData = playerData

rewardInteraction :: AdventureReward -> Maybe CompositeActionDefinition
rewardInteraction GrassReward = Just $ InteractionAction (BuildBuildingsInteraction [Grass]) []
rewardInteraction SmallPastureReward = Just $ InteractionAction (BuildBuildingsInteraction [SmallPasture]) [PayResources (wood 1)]
rewardInteraction _ = Nothing
