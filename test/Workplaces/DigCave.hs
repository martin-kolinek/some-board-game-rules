module Workplaces.DigCave where

import Rules
import TestFramework
import TestHelpers

import Data.AdditiveGroup
import Data.Map ((!))

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.Set as S

digCaveTests :: TestTree
digCaveTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Cut forest tests" $ [
    testProperty "Starting working makes correct decisions available" $ universeProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        decisions <- getsUniverse getPossibleDecisions <*> pure playerId
        assert $ (S.fromList decisions) == (S.fromList [CaveOrPassageOption ChooseCave, CaveOrPassageOption ChoosePassage, CaveOrPassageOption NoDigging]),
    testProperty "Starting working and choosing no digging ends turn" $ universeProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        checkPlayerHasValidOccupants playerId
        applyToUniverse $ chooseOption playerId (CaveOrPassageOption NoDigging)
        validateNextPlayer playerId,
    testProperty "Starting working, choosing digging and canceling ends turn" $ universeProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        checkPlayerHasValidOccupants playerId
        decision <- pick $ elements [CaveOrPassageOption ChooseCave, CaveOrPassageOption ChoosePassage]
        applyToUniverse $ chooseOption playerId decision
        applyToUniverse $ cancelSelection playerId
        validateNextPlayer playerId,
    testProperty "Starting working, and digging cave adds cave" $ universeProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        applyToUniverse $ chooseOption playerId (CaveOrPassageOption ChooseCave)
        (pos, dir) <- selectCorrectPosition availableRockPositions playerId
        buildings <- getsUniverse getBuildingSpace <*> pure playerId
        assert $ Building Cave pos `elem` buildings
        assert $ Building Cave (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working, and digging passage adds cave" $ universeProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        applyToUniverse $ chooseOption playerId (CaveOrPassageOption ChoosePassage)
        (pos, dir) <- selectCorrectPosition availableRockPositions playerId
        buildings <- getsUniverse getBuildingSpace <*> pure playerId
        assert $ Building Cave pos `elem` buildings
        assert $ Building Passage (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working adds stone" $ universeProperty $ do
        originalUniverse <- getUniverse
        (playerId, _, workplaceId) <- startWorkingInDigCave
        let originalStone = getStoneAmount $ (getPlayerResources originalUniverse playerId)
            workplaceAmount = getStoneAmount $ getWorkplaceResources $ getWorkplaces originalUniverse ! workplaceId
        newStone <- getStoneAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
        assert $ newStone == originalStone + workplaceAmount,
    testProperty "Starting working, choosing digging, and selecting invalid position fails" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigCave
      decision <- pick $ elements [CaveOrPassageOption ChooseCave, CaveOrPassageOption ChoosePassage]
      applyToUniverse $ chooseOption playerId decision
      _ <- selectWrongPosition availableRockPositions playerId
      shouldHaveFailed,
    testProperty "Starting working, choosing digging, and selecting correct position starts next player" $ universeProperty $ do
      (playerId, _, _) <- startWorkingInDigCave
      checkPlayerHasValidOccupants playerId
      decision <- pick $ elements [CaveOrPassageOption ChooseCave, CaveOrPassageOption ChoosePassage]
      applyToUniverse $ chooseOption playerId decision
      _ <- selectCorrectPosition availableRockPositions playerId
      validateNextPlayer playerId
  ]

startWorkingInDigCave :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInDigCave = startWorkingInWorkplaceType DigCave
