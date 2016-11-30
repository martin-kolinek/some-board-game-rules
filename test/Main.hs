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
    houseWorkTests]
