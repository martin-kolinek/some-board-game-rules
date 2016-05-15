import           Test.Tasty
import           UniverseTest
import BuildingTest
import RulesTest

main = defaultMain $ testGroup "Tests" [universeTests, buildingTests, rulesTests]
