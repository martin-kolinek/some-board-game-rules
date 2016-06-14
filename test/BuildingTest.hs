module BuildingTest where

import Data.Default
import Building
import TestFramework
import Control.Monad.State
import Test.Tasty
import Test.Tasty.HUnit as H

instance Default BuildingSpace where
  def = initialBuildingSpace

buildingTests = testGroup "Building" [
  flowTestCase "Cutting down a forest results in grass" $ do
    apply $ cutForest (0, 0) DirectionDown
    building <- getBuilding <$> get <*> pure (0, 0)
    liftIO $ Just (Grass (0, 0)) @=? building
    building <- getBuilding <$> get <*> pure (0, 1)
    liftIO $ Just (Grass (0, 1)) @=? building
    return ()
  ,
  flowTestCaseFailure "Building out of bounds fails" $
    apply $ cutForest (-5, -5) DirectionUp
  ,
  flowTestCaseFailure "Cutting down not forest fails" $
    apply $ cutForest(2, 0) DirectionUp
  ,
  testCase "Available building positions returns 8 positions" $
    liftIO $ 8 @=? length availableBuildingPositions
  ]
