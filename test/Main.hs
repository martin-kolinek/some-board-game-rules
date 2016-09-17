import Test.Tasty
import UniverseTest
import BuildingTest
import RulesTest
import ArbitraryUniverseTest

main :: IO ()
main = defaultMain $ do
  testGroup "Tests" [universeTests, buildingTests, rulesTests, arbitraryUniverseTests]
