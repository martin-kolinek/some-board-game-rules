module BuildingTest where

import Building
import TestFramework
import Control.Monad.State
import Test.Tasty
import Data.List
import Test.Tasty.HUnit as H

buildingTests :: TestTree
buildingTests = testGroup "Building" [
  flowTestCaseFailure initialBuildingSpace "Building out of bounds fails" $
    apply $ cutForest (-5, -5) DirectionUp
  ,
  flowTestCaseFailure initialBuildingSpace "Cutting down not forest fails" $
    apply $ cutForest(2, 0) DirectionUp
  ,
  testCase "Available building positions returns 8 positions" $
    liftIO $ 24 @=? length availableBuildingPositions
  ,
  testCase "Initial building space does not have overlapping buildings" $ do
    let initialBuildings = getBuildings initialBuildingSpace
        positions = buildingPositions =<< initialBuildings
    liftIO $ length positions @=? length (nub positions)
  ]
