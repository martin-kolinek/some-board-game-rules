module Workplaces.WeaponMaking where

import Rules
import TestFramework
import TestHelpers

import Test.Tasty.QuickCheck
import Test.Tasty
import Test.QuickCheck.Monadic

weaponMakingTests :: TestTree
weaponMakingTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Weapon making tests" $ [
    testProperty "Can arm worker after starting working and worker is unarmed" $ movingWorkerProperty $ do
        (plId, workerId, _) <- startWorkingInWeaponMaking
        canArm <- getsUniverse isArmingWorker <*> pure plId
        checkWorkerIsUnarmed workerId
        assert $ canArm,
    testProperty "Can go on adventure after starting working and arming" $ movingWorkerProperty $ do
        (plId, workerId, _) <- startWorkingInWeaponMaking
        checkWorkerIsUnarmed workerId
        applyToUniverse $ armWorker plId 1
        canAdventure <- getsUniverse canGoOnAdventure <*> pure plId
        assert $ canAdventure,
    testProperty "Can arm worker after starting working and worker is unarmed" $ movingWorkerProperty $ do
        (plId, workerId, _) <- startWorkingInWeaponMaking
        canArm <- getsUniverse isArmingWorker <*> pure plId
        checkWorkerIsUnarmed workerId
        assert $ canArm,
    testProperty "Next player's turn after arming and adventuring" $ movingWorkerProperty $ do
      (plId, workerId, _) <- startWorkingInWeaponMaking
      checkWorkerIsUnarmed workerId
      checkPlayerHasValidOccupants plId
      applyToUniverse $ armWorker plId 1
      applyToUniverse $ adventure plId WoodReward
      validateNextPlayer plId,
    testProperty "Next player's turn after adventuring" $ movingWorkerProperty $ do
      (plId, workerId, _) <- startWorkingInWeaponMaking
      checkWorkerIsArmed workerId
      checkPlayerHasValidOccupants plId
      applyToUniverse $ adventure plId WoodReward
      validateNextPlayer plId,
    testProperty "Next player's turn after adventuring and building" $ movingWorkerProperty $ do
      (plId, workerId, _) <- startWorkingInWeaponMaking
      checkWorkerIsArmed workerId
      checkPlayerHasValidOccupants plId
      (pos, dir) <- pickSpecificPosition availableSingleForestPositions plId
      applyToUniverse $ adventure plId GrassReward
      applyToUniverse $ buildBuildings plId pos dir $ SingleSmallBuildingDesc Grass
      validateNextPlayer plId
  ]

startWorkingInWeaponMaking :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInWeaponMaking = do
  universe <- getUniverse
  (plId, workerId, workplaceId) <- startWorkingInWorkplaceType WeaponMaking
  let iron = getIronAmount $ getPlayerResources universe plId
  let workerStrength = getWorkerStrength universe workerId
  pre $ workerStrength > 0 || iron > 0
  return (plId, workerId, workplaceId)

checkWorkerIsUnarmed :: WorkerId -> UniversePropertyMonad ()
checkWorkerIsUnarmed workerId = do
  workerStrength <- getsUniverse getWorkerStrength <*> pure workerId
  pre $ workerStrength == 0

checkWorkerIsArmed :: WorkerId -> UniversePropertyMonad ()
checkWorkerIsArmed workerId = do
  workerStrength <- getsUniverse getWorkerStrength <*> pure workerId
  pre $ workerStrength > 0
