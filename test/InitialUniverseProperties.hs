module InitialUniverseProperties where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Map as M
import Rules
import Data.AdditiveGroup
import Data.List

initialUniverseTests :: TestTree
initialUniverseTests = testGroup "Initial universe tests"
  [
    testProperty "There are workplaces with resources" $
      let workplaceResources = getWorkplaceResources <$> M.elems (getWorkplaces initialUniverse)
      in any (/= zeroV) workplaceResources,
    testProperty "There are workplaces of all types" $
      let workplaceTypes = nub $ sort $ getWorkplaceType <$> M.elems (getWorkplaces initialUniverse)
      in workplaceTypes == [CutForest ..],
    testProperty "There are no more than 40 workplaces" $
      M.size (getWorkplaces initialUniverse) <= 40
  ]
