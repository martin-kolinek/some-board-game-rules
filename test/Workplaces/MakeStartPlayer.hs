module Workplaces.MakeStartPlayer where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import TestFramework

import Rules

startPlayerTests :: TestTree
startPlayerTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Make start player tests" $ [
    testProperty "Next player moves after working" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInStartPlayer
      checkPlayerHasValidOccupants playerId
      validateNextPlayer playerId,
    testProperty "Player is starting player after working" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInStartPlayer
      startingPlayer <- getsUniverse getStartingPlayer
      assert $ startingPlayer == playerId
  ]

isStartPlayer :: WorkplaceData -> Bool
isStartPlayer (MakeStartPlayer _) = True
isStartPlayer _ = False

startWorkingInStartPlayer :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInStartPlayer = startWorkingInWorkplaceType isStartPlayer
