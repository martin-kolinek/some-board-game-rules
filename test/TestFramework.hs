{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module TestFramework where

import           Test.Tasty
import           Test.Tasty.HUnit as H
import Control.Monad.State
import Rules
import Data.Map.Strict
import Control.Monad.Except
import Building
import Data.Default

type FlowTest t = ExceptT String (StateT t IO)

flowTestCase :: Default a => String -> FlowTest a () -> TestTree
flowTestCase = flowTestCaseWithCheck check
  where check (Left msg) = assertFailure $ "No error expected, but got: " ++ msg
        check _ = return ()

flowTestCaseFailure :: Default a => String -> FlowTest a () -> TestTree
flowTestCaseFailure = flowTestCaseWithCheck check
  where check (Right _) = assertFailure "Expected error, but got none"
        check _ = return ()

flowTestCaseWithCheck check name action = testCase name $ do
  (result, s) <- runStateT (runExceptT action) def
  check result
  return ()

getPlayer :: Int -> FlowTest Universe PlayerId
getPlayer number = (!! number) . getPlayers <$> get

player1 = getPlayer 0
player2 = getPlayer 1

getWorker playerNumber workerNumber = (!! workerNumber) <$> (getWorkers <$> get <*> getPlayer playerNumber)

getWorkplace number = (!! number) . keys . getWorkplaces <$> get

apply :: (forall m. MonadError String m => (a -> m a)) -> FlowTest a ()
apply action = put =<< action =<< get
