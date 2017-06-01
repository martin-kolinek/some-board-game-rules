module Workplaces.WeaponMaking where

import Rules
import TestFramework
import TestHelpers

import Test.Tasty.QuickCheck
import Test.Tasty
import Test.QuickCheck.Monadic

weaponMakingTests :: TestTree
weaponMakingTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Cut forest tests" $ [
    testProperty "Can arm worker after starting working and worker is unarmed" $ universeProperty $ do
        (plId, _, _) <- startWorkingInWeaponMaking
        canArm <- getsUniverse isArmingWorker <*> pure plId
        assert $ canArm,
    testProperty "Next player's turn after arming" $ universeProperty $ do
      (plId, _, _) <- startWorkingInWeaponMaking
      checkPlayerHasValidOccupants plId
      applyToUniverse $ armWorker plId 1
      validateNextPlayer plId
  ]

startWorkingInWeaponMaking :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInWeaponMaking = do
  universe <- getUniverse
  (plId, workerId, workplaceId) <- startWorkingInWorkplaceType WeaponMaking
  let iron = getIronAmount $ getPlayerResources universe plId
  let workerStrength = getWorkerStrength universe workerId
  pre $ workerStrength == 0
  pre $ iron > 0
  return (plId, workerId, workplaceId)


