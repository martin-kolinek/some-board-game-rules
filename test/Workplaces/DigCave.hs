module Workplaces.DigCave where

import Rules

import Data.AdditiveGroup
import Data.Map ((!))

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import TestFramework
import RulesProperties

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
        applyToUniverse $ chooseOption (CaveOrPassageOption NoDigging)
        validateNextPlayer playerId,
    testProperty "Starting working, choosing digging and canceling ends turn" $ universeProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        checkPlayerHasValidOccupants playerId
        decision <- pick $ elements [CaveOrPassageOption ChooseCave, CaveOrPassageOption ChoosePassage]
        applyToUniverse $ chooseOption decision
        applyToUniverse $ cancelSelection
        validateNextPlayer playerId,
    testProperty "Starting working, and digging cave adds cave" $ universeProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        applyToUniverse $ chooseOption (CaveOrPassageOption ChooseCave)
        (pos, dir) <- selectCorrectPosition availableRockPositions playerId
        buildings <- getsUniverse getBuildingSpace <*> pure playerId
        assert $ Cave pos `elem` buildings
        assert $ Cave (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working, and digging passage adds cave" $ universeProperty $ do
        (playerId, _, _) <- startWorkingInDigCave
        applyToUniverse $ chooseOption (CaveOrPassageOption ChoosePassage)
        (pos, dir) <- selectCorrectPosition availableRockPositions playerId
        buildings <- getsUniverse getBuildingSpace <*> pure playerId
        assert $ Cave pos `elem` buildings
        assert $ Passage (pos ^+^ directionAddition dir) `elem` buildings,
    testProperty "Starting working adds stone" $ universeProperty $ do
        originalUniverse <- getUniverse
        (playerId, _, workplaceId) <- startWorkingInDigCave
        let originalStone = getStoneAmount $ (getPlayerResources originalUniverse playerId)
            DigCave workplaceAmount = getWorkplaces originalUniverse ! workplaceId
        newStone <- getStoneAmount <$> (getsUniverse getPlayerResources <*> pure playerId)
        assert $ newStone == originalStone + workplaceAmount
  ]

isDigCave :: WorkplaceData -> Bool
isDigCave (DigCave _) = True
isDigCave _ = False

startWorkingInDigCave :: UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInDigCave = startWorkingInWorkplaceType isDigCave
