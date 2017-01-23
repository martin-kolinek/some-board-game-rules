module Workplaces.MakeStartPlayer where

import Rules
import TestFramework
import TestHelpers

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

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

startWorkingInStartPlayer :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInStartPlayer = startWorkingInWorkplaceType MakeStartPlayer
