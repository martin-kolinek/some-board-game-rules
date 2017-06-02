module Main where

import Test.Tasty
import ArbitraryUniverseTest
import RulesProperties
import Workplaces.CutForest
import Workplaces.DigPassage
import Workplaces.DigCave
import Workplaces.WorkerNeed
import Workplaces.ResourceAddition
import Workplaces.GatherWood
import Workplaces.GatherFood
import Workplaces.MakeStartPlayer
import Workplaces.HouseWork
import Workplaces.Farming
import Workplaces.WeaponMaking
import BuildingProperties
import Interaction.FarmingProperties
import Interaction.ArmingProperties
import Interaction.AdventureProperties
import InitialUniverseProperties

main :: IO ()
main = defaultMain $ do
  testGroup "Tests" [
    arbitraryUniverseTests,
    rulesPropertiesTests,
    cutForestTests,
    digPassageTests,
    digCaveTests,
    workerNeedTests,
    resourceAdditionTests,
    gatherWoodTests,
    gatherFoodTests,
    startPlayerTests,
    houseWorkTests,
    farmingTests,
    farmingWorkplaceTests,
    buildingTests,
    initialUniverseTests,
    armingTests,
    weaponMakingTests,
    adventureTests]
