module Universe.Adventure where

import Player
import Resources
import Actions
import Building
import Universe
import Universe.Actions

import Control.Lens hiding (universe)

data AdventureReward = WoodReward | GrassReward | SmallPastureReward | SheepReward | LargePastureReward

applyReward :: AdventureReward -> Universe -> PlayerData -> PlayerData
applyReward WoodReward _ playerData = playerData & (playerResources . woodAmount) +~ 1
applyReward SheepReward universe playerData = addAnimal (FarmAnimalType Sheep) universe playerData
applyReward _ _ playerData = playerData

rewardInteraction :: AdventureReward -> Maybe CompositeActionDefinition
rewardInteraction GrassReward = Just $ InteractionAction (BuildBuildingsInteraction (SingleSmallBuildingDesc Grass)) []
rewardInteraction SmallPastureReward = Just $ InteractionAction (BuildBuildingsInteraction (SingleSmallBuildingDesc SmallPasture)) [PayResources (wood 1)]
rewardInteraction LargePastureReward = Just $ InteractionAction (BuildBuildingsInteraction (LargeBuildingDesc LargePasture)) [PayResources (wood 2)]
rewardInteraction _ = Nothing
