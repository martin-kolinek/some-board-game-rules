module Main where

import Test.Tasty
import UniverseTest
import BuildingTest
import ArbitraryUniverseTest
import RulesProperties

main :: IO ()
main = defaultMain $ do
  testGroup "Tests" [universeTests, buildingTests, arbitraryUniverseTests, rulesPropertiesTests]
