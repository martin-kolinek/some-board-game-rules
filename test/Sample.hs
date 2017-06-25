module Main where

import Generators
import TestFramework
import Test.QuickCheck
import Control.Monad
import Text.Show.Pretty

main :: IO ()
main = do
  let props = defaultGeneratorProperties
  x <- sample' $ generateUniverse props
  forM_ x $ \a -> pPrint a
