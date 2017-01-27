module Workplaces.WeaponMaking where

import Rules
import TestFramework
import TestHelpers

import Test.Tasty.QuickCheck
import Test.Tasty
import Test.QuickCheck.Monadic

weaponMakingTests :: TestTree
weaponMakingTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Cut forest tests" $ [
    testProperty "Options for strengths to 8 are available" $ universeProperty $ do
      (plId, _, _) <- startWorkingInWeaponMaking
      options <- getsUniverse getPossibleDecisions <*> pure plId
      assert (options == (ArmOption <$> [0..8])),
    testProperty "Next player's turn after deciding" $ universeProperty $ do
      (plId, _, _) <- startWorkingInWeaponMaking
      checkPlayerHasValidOccupants plId
      chosenOption <- pick . elements =<< getsUniverse getPossibleDecisions <*> pure plId
      applyToUniverse $ chooseOption plId chosenOption
      validateNextPlayer plId,
    testProperty "Choosing strength adds strength" $ universeProperty $ do
      (plId, workerId, _) <- startWorkingInWeaponMaking
      strength <- pick $ elements [0..8]
      originalStrength <- getsUniverse getWorkerStrength <*> pure workerId
      applyToUniverse $ chooseOption plId $ ArmOption strength
      newStrength <- getsUniverse getWorkerStrength <*> pure workerId
      assert $ newStrength == originalStrength + strength
  ]

startWorkingInWeaponMaking :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInWeaponMaking = startWorkingInWorkplaceType WeaponMaking
