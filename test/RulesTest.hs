module RulesTest where

import Rules
import TestFramework

import Test.Tasty
import Test.Tasty.HUnit
import Data.Default
import Control.Monad.State
import qualified Data.Map as M

instance Default Universe where
  def = initialUniverse

rulesTests = testGroup "Rules" [
    flowTestCase "A game can be played" $ do
      workers <- gets getWorkers <*> player1
      liftIO $ 2 @=? length workers
      workers <- gets getWorkers <*> player2
      liftIO $ 3 @=? length workers
      workplace <- gets getWorkerWorkplace <*> getWorker 0 0
      liftIO $ Nothing @=? workplace
      worker <- getWorker 0 0
      workplace <- getWorkplace 0
      apply $ startWorking worker workplace
      workers <- gets getWorkplaceOccupants <*> pure workplace
      liftIO $ [worker] @=? workers
      worker <- getWorker 1 0
      workplace <- getWorkplace 1
      buildings <- gets getBuildingSpace <*> getPlayer 1
      liftIO $ 24 @=? length buildings
      liftIO $ assertBool "no forest" $ Forest (0, 0) `elem` buildings
      liftIO $ assertBool "no rock" $ Rock (5, 0) `elem` buildings
      liftIO $ assertBool "no initial room" $ InitialRoom (3, 3) `elem` buildings
      occupants <- gets getBuildingOccupants <*> getPlayer 1
      liftIO $ assertBool "Contains worker" $ WorkerOccupant worker `elem` (occupants M.! (3, 3))
      allOccupants <- gets getAllOccupants <*> getPlayer 1
      player <- player2
      apply $ alterOccupants player (M.fromList [((2, 1), allOccupants)])
      errors <- gets getOccupantErrors <*> player2
      liftIO $ 1 @=? length errors
      apply $ startWorking worker workplace
      curPlayer <- gets getCurrentPlayer
      liftIO $ Just player @=? curPlayer
      apply $ alterOccupants player occupants
      worker <- getWorker 0 1
      workplace <- getWorkplace 2
      apply $ startWorking worker workplace
      worker <- getWorker 1 1
      workplace <- getWorkplace 3
      apply $ startWorking worker workplace
      worker <- getWorker 1 2
      workplace <- getWorkplace 4
      apply $ startWorking worker workplace
      apply finishTurn
      worker <- getWorker 0 0
      workplace <- getWorkplace 6
      apply $ startWorking worker workplace
      apply cancelSelection
      apply $ startWorking worker workplace
      status <- getPlayerStatus <$> get <*> player1
      liftIO $ CuttingForest @=? status
      apply $ selectPosition (0, 0) DirectionDown
      buildings <- gets getBuildingSpace <*> player1
      liftIO $ 24 @=? length availableBuildingPositions
      liftIO $ assertBool "no grass" $ Grass (0, 0) `elem` buildings
      resources <- gets getPlayerResources <*> getPlayer 0
      liftIO $ 0 @=? getWoodAmount resources
      return ()
  ]
