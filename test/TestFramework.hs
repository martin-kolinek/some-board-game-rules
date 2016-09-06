{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module TestFramework where

import Rules

import Test.Tasty
import Test.Tasty.HUnit as H
import Control.Monad.State
import Data.Map.Strict
import Control.Monad.Except

type FlowTest t = ExceptT String (StateT t IO)

flowTestCase :: a -> String -> FlowTest a () -> TestTree
flowTestCase = flowTestCaseWithCheck check
  where check (Left msg) = assertFailure $ "No error expected, but got: " ++ msg
        check _ = return ()

flowTestCaseFailure :: a -> String -> FlowTest a () -> TestTree
flowTestCaseFailure = flowTestCaseWithCheck check
  where check (Right _) = assertFailure "Expected error, but got none"
        check _ = return ()

flowTestCaseWithCheck :: (Either String a -> IO ()) -> u -> TestName -> FlowTest u a -> TestTree
flowTestCaseWithCheck check initial name action = testCase name $ do
  (result, _) <- runStateT (runExceptT action) initial
  check result
  return ()

getPlayer :: Int -> FlowTest Universe PlayerId
getPlayer number = (!! number) . getPlayers <$> get

player1 :: FlowTest Universe PlayerId
player1 = getPlayer 0

player2 :: FlowTest Universe PlayerId
player2 = getPlayer 1

getWorker :: Int -> Int -> FlowTest Universe WorkerId
getWorker playerNumber workerNumber = (!! workerNumber) <$> (getWorkers <$> get <*> getPlayer playerNumber)

getWorkplace :: MonadState Universe f => Int -> f WorkplaceId
getWorkplace number = (!! number) . keys . getWorkplaces <$> get

apply :: (forall m. MonadError String m => (a -> m a)) -> FlowTest a ()
apply action = put =<< action =<< get
