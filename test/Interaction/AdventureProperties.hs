module Interaction.AdventureProperties where

import Rules
import TestFramework
import TestHelpers

import Test.Tasty.QuickCheck
import Test.Tasty
import Test.QuickCheck.Monadic
import Data.Maybe
import Data.Function ((&))

adventureTests :: TestTree
adventureTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Adventure tests" [
  testProperty "Wood reward adds wood" $ adventureProperty $ do
    plId <- findAdventuringPlayer
    origWood <- fmap getWoodAmount $ getsUniverse getPlayerResources <*> pure plId
    applyToUniverse $ adventure plId WoodReward
    newWood <- fmap getWoodAmount $ getsUniverse getPlayerResources <*> pure plId
    assert $ newWood == origWood + 1,
  testProperty "Cannot choose grass reward when there is no space" $ adventureProperty $ do
    plId <- findAdventuringPlayer
    forestPositions <- getsUniverse availableSingleForestPositions <*> pure plId
    pre $ forestPositions == []
    applyToUniverse $ adventure plId GrassReward
    shouldHaveFailed,
  testProperty "Can build grass after choosing grass reward" $ adventureProperty $ do
    plId <- findAdventuringPlayer
    forestPositions <- getsUniverse availableSingleForestPositions <*> pure plId
    pre $ forestPositions /= []
    applyToUniverse $ adventure plId GrassReward
    builtBuildings <- getsUniverse currentlyBuiltBuildings <*> pure plId
    assert $ builtBuildings == [[Grass]],
  testProperty "Building works after adventuring" $ adventureProperty $ do
    plId <- findAdventuringPlayer
    (pos, dir) <- pickSpecificPosition availableSingleForestPositions plId
    applyToUniverse $ adventure plId GrassReward
    applyToUniverse $ buildBuildings plId pos dir [Grass]
    buildings <- getsUniverse getBuildingSpace <*> pure plId
    assert $ (Building Grass pos) `elem` buildings
  ]

findAdventuringPlayer :: UniversePropertyMonad PlayerId
findAdventuringPlayer = do
  universe <- getUniverse
  plId <- preMaybe $ listToMaybe [plId | plId <- getPlayers universe, canGoOnAdventure universe plId]
  checkPlayerHasValidOccupants plId
  return plId

adventureProperty :: UniversePropertyMonad a -> Property
adventureProperty = propertyWithProperties $ defaultGeneratorProperties &
  withWorkplaceProbability WeaponMaking 20 &
  withAdventureProbability 20 &
  withOtherWorkersNotDoneProbability 20
