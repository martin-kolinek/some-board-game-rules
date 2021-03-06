module TestHelpers where

import TestFramework
import Rules
import qualified Data.Set as S
import Control.Monad
import Data.Maybe
import Data.Map hiding (null, filter)
import Test.QuickCheck.Monadic
import Test.QuickCheck
import Data.List (intersect)
import Data.AdditiveGroup

startWorkingInWorkplaceType :: WorkplaceType -> UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInWorkplaceType wType = do
  let extractFreeWorkers playerId universe = do
        workerId <- getWorkers universe playerId
        guard $ isNothing $ getWorkerWorkplace universe workerId
        return workerId
      extractFilledWorkplaces universe = do
        playerId <- getPlayers universe
        workerId <- getWorkers universe playerId
        maybeToList $ getWorkerWorkplace universe workerId
      extractAppropriateWorkpalces universe = filterWithKey filterFunc (getWorkplaces universe)
        where filterFunc workplaceId workplaceData =
                getWorkplaceType workplaceData == wType && not (S.member workplaceId filledWorkplaceIds)
              filledWorkplaceIds = S.fromList (extractFilledWorkplaces universe)
  currentPlayerId <- preMaybe =<< getsUniverse getCurrentPlayer
  checkPlayerHasValidOccupants currentPlayerId
  pre =<< getsUniverse isMovingWorker <*> pure currentPlayerId
  freeWorkers <- getsUniverse $ extractFreeWorkers currentPlayerId
  pre $ not $ null $ freeWorkers
  selectedWorkerId <- pick $ elements freeWorkers
  appropriateWorkplaces <- getsUniverse extractAppropriateWorkpalces
  pre $ not $ null $ appropriateWorkplaces
  selectedWorkplaceId <- pick $ elements $ keys appropriateWorkplaces
  applyToUniverse $ startWorking currentPlayerId selectedWorkerId selectedWorkplaceId
  return (currentPlayerId, selectedWorkerId, selectedWorkplaceId)

pickSpecificPosition :: (Universe -> PlayerId -> [(Position, Direction)]) -> PlayerId -> UniversePropertyMonad (Position, Direction)
pickSpecificPosition func plId = do
  positions <- getsUniverse func <*> pure plId
  pre $ not $ null $ positions
  pick $ elements positions

pickWrongPosition :: (Universe -> PlayerId -> [(Position, Direction)]) -> PlayerId -> UniversePropertyMonad (Position, Direction)
pickWrongPosition func plId = do
  let allPositions = [((x, y), dir) | x <- [-1..6], y <- [-1..4], dir <- allDirections]
  positions <- getsUniverse func <*> pure plId
  pick $ elements $ S.toList $ S.fromList allPositions S.\\ S.fromList positions

pickBuildingsToBuild :: PlayerId -> UniversePropertyMonad BuildingDescription
pickBuildingsToBuild plId = do
  buildingOptions <- getsUniverse currentlyBuiltBuildings <*> pure plId
  assert $ buildingOptions /= []
  pick $ elements $ buildingOptions

selectWrongPosition :: (Universe -> PlayerId -> [(Position, Direction)]) -> PlayerId -> UniversePropertyMonad (Position, Direction)
selectWrongPosition func plId = do
  (pos, dir) <- pickWrongPosition func plId
  buildings <- pickBuildingsToBuild plId
  applyToUniverse $ buildBuildings plId pos dir buildings
  return (pos, dir)

selectCorrectPosition :: (Universe -> PlayerId -> [(Position, Direction)]) -> PlayerId -> UniversePropertyMonad (Position, Direction)
selectCorrectPosition func plId = do
  (pos, dir) <- pickSpecificPosition func plId
  buildings <- pickBuildingsToBuild plId
  applyToUniverse $ buildBuildings plId pos dir buildings
  return (pos, dir)

nextPlayerToMoveWorker :: Universe -> PlayerId -> Maybe PlayerId
nextPlayerToMoveWorker universe currentPlayerId =
  let furtherPlayerIds = tail $ dropWhile (/= currentPlayerId) $ getPlayers universe ++ getPlayers universe
      isWorkerFree workerId = isNothing (getWorkerWorkplace universe workerId)
      playersWithFreeWorkers = [plId | plId <- furtherPlayerIds, any isWorkerFree (getWorkers universe plId)]
  in listToMaybe playersWithFreeWorkers

validateNextPlayer :: PlayerId -> UniversePropertyMonad ()
validateNextPlayer previousPlayerId = do
  nextPlayerId <- getsUniverse nextPlayerToMoveWorker <*> pure (previousPlayerId)
  currentPlayerId <- getsUniverse getCurrentPlayer
  assert $ currentPlayerId == nextPlayerId

checkPlayerHasValidOccupants :: PlayerId -> UniversePropertyMonad ()
checkPlayerHasValidOccupants plId = pre =<< null <$> (getsUniverse getOccupantErrors <*> pure plId)

checkPlayerHasInvalidOccupants :: PlayerId -> UniversePropertyMonad ()
checkPlayerHasInvalidOccupants plId = pre =<< (not . null) <$> (getsUniverse getOccupantErrors <*> pure plId)

validatePlayerHasValidOccupants :: PlayerId -> UniversePropertyMonad ()
validatePlayerHasValidOccupants plId = assert =<< null <$> (getsUniverse getOccupantErrors <*> pure plId)

availableForestPositions :: Universe -> PlayerId -> [(Position, Direction)]
availableForestPositions universe playerId = filter isNotOverlappingBarn positions
  where positions = availableSpecificPositions isCuttable isDevelopedOutside False universe playerId
        isNotOverlappingBarn (pos, dir) = not $ (pos ^+^ directionAddition dir) `elem` getBarns universe playerId

availableSingleForestPositions :: Universe -> PlayerId -> [(Position, Direction)]
availableSingleForestPositions = availableSpecificPositions isCuttable isDevelopedOutside True

isCuttable :: Foldable t => t Building -> Position -> Bool
isCuttable buildingSpace pos = SmallBuilding Forest pos `elem` buildingSpace

availableSingleGrassPositions :: Universe -> PlayerId -> [(Position, Direction)]
availableSingleGrassPositions = availableSpecificPositions isGrass isDevelopedOutside True

availableGrassPositions :: Universe -> PlayerId -> [(Position, Direction)]
availableGrassPositions = availableSpecificPositions isGrass isDevelopedOutside False

isGrass :: Foldable t => t Building -> Position -> Bool
isGrass buildingSpace pos = SmallBuilding Grass pos `elem` buildingSpace

isDevelopedOutside :: [Building] -> Position -> Bool
isDevelopedOutside buildingSpace pos = not $ null $
  intersect (small ++ centeredLarge ++ offsetLarge) buildingSpace
  where centeredLarge = [LargeBuilding LargePasture pos dir | dir <- allDirections]
        offsetLarge = [LargeBuilding LargePasture (pos ^-^ directionAddition dir) dir | dir <- allDirections]
        small = [SmallBuilding Field pos, SmallBuilding Grass pos, SmallBuilding InitialRoom pos, SmallBuilding SmallPasture pos]

availableRockPositions :: Universe -> PlayerId -> [(Position, Direction)]
availableRockPositions = availableSpecificPositions isDiggable isDevelopedInside False

availableSingleRockPositions :: Universe -> PlayerId -> [(Position, Direction)]
availableSingleRockPositions = availableSpecificPositions isDiggable isDevelopedInside True

isDiggable :: Foldable t => t Building -> Position -> Bool
isDiggable buildingSpace pos = SmallBuilding Rock pos `elem` buildingSpace

isDevelopedInside :: [Building] -> Position -> Bool
isDevelopedInside buildingSpace pos = not $ null $ intersect [SmallBuilding InitialRoom pos, SmallBuilding Cave pos, SmallBuilding Passage pos, SmallBuilding LivingRoom pos] buildingSpace

availableSingleCavePositions :: Universe -> PlayerId -> [(Position, Direction)]
availableSingleCavePositions = availableSpecificPositions isBuildable (const $ const True) True
  where isBuildable buildingSpace pos = SmallBuilding Cave pos `elem` buildingSpace

availableSpecificPositions :: ([Building] -> Position -> Bool) -> ([Building] -> Position -> Bool) -> Bool -> Universe -> PlayerId -> [(Position, Direction)]
availableSpecificPositions freeCondition developedCondition ignoreDirection universe playerId = [(pos, direction) |
                             direction <- allDirections,
                             pos <- availableBuildingPositions,
                             freeCondition buildingSpace pos,
                             freeCondition buildingSpace (pos ^+^ directionAddition direction) || ignoreDirection,
                             neighbourPositionsReachable $ if ignoreDirection then [pos] else [pos, pos ^+^ directionAddition direction]]
  where neighbourPositionsReachable positions = any (developedCondition buildingSpace) [pos ^+^ directionAddition dir | pos <- positions, dir <- allDirections]
        buildingSpace = currentPlayerBuildingSpace universe playerId

currentPlayerBuildingSpace :: Universe -> PlayerId -> [Building]
currentPlayerBuildingSpace universe playerId = getBuildingSpace universe playerId

currentPlayerHasEnoughResourcesForLivingRoom :: Universe -> Bool
currentPlayerHasEnoughResourcesForLivingRoom universe = fromMaybe False $ do
  currentPlayerId <- getCurrentPlayer universe
  let resources = getPlayerResources universe currentPlayerId
  return $ getWoodAmount resources >= 4 && getStoneAmount resources >= 3

currentPlayerHasFreeRoom :: Universe -> Bool
currentPlayerHasFreeRoom universe = fromMaybe False $ do
  currentPlayerId <- getCurrentPlayer universe
  let buildingSpace = getBuildingSpace universe currentPlayerId
      buildingCount (SmallBuilding LivingRoom _) = 1
      buildingCount (SmallBuilding InitialRoom _) = 2
      buildingCount _ = 0
      totalRoom = sum $ buildingCount <$> buildingSpace
  return (totalRoom > (length $ getWorkers universe currentPlayerId))

currentPlayerCanBuildRoom :: Universe -> Bool
currentPlayerCanBuildRoom universe = (not $ null $ join $ maybeToList $ availableSingleCavePositions universe <$> getCurrentPlayer universe) &&
  currentPlayerHasEnoughResourcesForLivingRoom universe

currentPlayerCanArmWorker :: WorkerId -> Universe -> Bool
currentPlayerCanArmWorker workerId universe = fromMaybe False $ do
  currentPlayerId <- getCurrentPlayer universe
  let hasSomeIron = getIronAmount (getPlayerResources universe currentPlayerId) > 0
      workerIsUnarmed = getWorkerStrength universe workerId == 0
  return $ hasSomeIron && workerIsUnarmed

findValidBarnPositions :: Universe -> PlayerId -> [Position]
findValidBarnPositions universe plId = filter hasNoBarn $ validBuildingPosition =<< getBuildingSpace universe plId
  where validBuildingPosition (SmallBuilding Grass pos) = [pos]
        validBuildingPosition (SmallBuilding Forest pos) = [pos]
        validBuildingPosition (SmallBuilding SmallPasture pos) = [pos]
        validBuildingPosition (LargeBuilding LargePasture pos dir) = [pos, pos ^+^ directionAddition dir]
        validBuildingPosition _ = []
        hasNoBarn pos = not $ pos `elem` getBarns universe plId

checkCanBuildBarn :: PlayerId -> UniversePropertyMonad ()
checkCanBuildBarn plId = do
  barns <- getsUniverse getBarns <*> pure plId
  pre $ length barns < 2
