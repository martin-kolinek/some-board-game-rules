import           Test.Tasty
import           UniverseTest
import BuildingTest

main = defaultMain $ testGroup "Tests" [universeTests, buildingTests]
