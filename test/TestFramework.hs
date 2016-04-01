{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module TestFramework where

import           Test.Tasty
import           Test.Tasty.HUnit as H
import Control.Monad.State
import Rules
import Data.Map.Strict
import Control.Monad.Except

type UniverseTest = ExceptT String (StateT Universe IO)

universeTestCase :: String -> UniverseTest () -> TestTree
universeTestCase = universeTestCaseWithCheck check
  where check (Left msg) = assertFailure $ "No error expected, but got: " ++ msg
        check _ = return ()

universeTestCaseFailure :: String -> UniverseTest () -> TestTree
universeTestCaseFailure = universeTestCaseWithCheck check
  where check (Right _) = assertFailure "Expected error, but got none"
        check _ = return ()

universeTestCaseWithCheck check name action = testCase name $ do
  (result, s) <- runStateT (runExceptT action) initialUniverse
  check result
  return ()

readUniverse :: UniverseTest Universe
readUniverse = get

getPlayer number = (!! number) . getPlayers <$> readUniverse

player1 = getPlayer 0
player2 = getPlayer 1

getWorker playerNumber workerNumber = (!! workerNumber) <$> (getWorkers <$> readUniverse <*> getPlayer playerNumber)

getWorkplace number = (!! number) . keys . getWorkplaces <$> readUniverse

applyToUniverse :: (forall m. MonadError String m => (Universe -> m Universe)) -> UniverseTest ()
applyToUniverse action = put =<< action =<< readUniverse
