module Main where

import Test.Tasty
import UniverseTest
import BuildingTest
import ArbitraryUniverseTest
import RulesProperties
import Workplaces.CutForest
import Workplaces.DigPassage
import Workplaces.DigCave
import Workplaces.WorkerNeed
import Workplaces.ResourceAddition
import Workplaces.GatherWood

main :: IO ()
main = defaultMain $ do
  testGroup "Tests" [
    universeTests,
    buildingTests,
    arbitraryUniverseTests,
    rulesPropertiesTests,
    cutForestTests,
    digPassageTests,
    digCaveTests,
    workerNeedTests,
    resourceAdditionTests,
    gatherWoodTests]
