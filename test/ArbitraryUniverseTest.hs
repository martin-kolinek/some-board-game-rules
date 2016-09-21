module ArbitraryUniverseTest where

import Control.Lens hiding (universe, elements)
import Test.Tasty.QuickCheck
import Test.Tasty
import Data.List (nub, (\\))
import Data.Map (toList, keys)
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import qualified Data.Set as S
import Data.Either (isLeft)

import Universe
import Building
import Player
import Generators
import Worker
import Workplace
import Universe.Building
import Universe.Actions

arbitraryUniverseTests :: TestTree
arbitraryUniverseTests = testGroup "Arbitrary universe tests" [
    testProperty "Generated buildings don't overlap" $
      let noBuildingsOverlap buildings = positions == (nub positions)
            where positions = [pos | building <- getBuildings buildings, pos <- buildingPositions building]
      in forAll generateBuildingSpace noBuildingsOverlap,
    testProperty "Generate buildings have all positions" $
      let buildingsHaveAllPositions buildings = S.fromList positions == S.fromList [(x, y)| x <- [0..5], y <- [0..3]]
            where positions = [pos | building <- getBuildings buildings, pos <- buildingPositions building]
      in forAll generateBuildingSpace buildingsHaveAllPositions,
    testProperty "Universe has at most 1 active player" $
      let isActive playerData = playerData ^. playerStatus /= Waiting
          prop (ArbitraryUniverse universe) = (lengthOf (players . traverse . filtered isActive) universe) <= 1
      in prop,
    testProperty "No two workers are in the same workplace" $
      let prop (ArbitraryUniverse universe) = workplaces == nub workplaces
            where workplaces = toListOf (players . traverse . workers . traverse . currentWorkplace . traverse) universe
      in prop,
    testProperty "No two buildings overlap" $
      let noBuildingsOverlap buildings = positions == nub positions
            where positions = [pos | building <- getBuildings buildings, pos <- buildingPositions building]
          prop (ArbitraryUniverse universe) = allOf (players . traverse . buildingSpace) noBuildingsOverlap universe
      in prop,
    testProperty "When all players are waiting then all workers are busy" $
      let allWorkersBusy = allOf (players . traverse . workers . traverse . currentWorkplace) isJust
          prop (ArbitraryUniverse universe) = allPlayersWaiting universe ==> allWorkersBusy universe
      in prop,
    testProperty "If a player is in InvalidOccupants state then he has invalid occupants" $
      let findPlayerWithOccupantsInvalid universe = universe ^?
            (players . to toList . traverse . filtered (notNullOf (_2 . playerStatus . filtered (==OccupantsInvalid))) . _1)
          prop (ArbitraryUniverse universe) = isJust currentPlayerId ==> not . null $ getOccupantErrors universe (fromJust currentPlayerId)
            where currentPlayerId = findPlayerWithOccupantsInvalid universe
      in prop,
    testProperty "If a player is in CuttingForest state then he has a most recent worker which is in cut forest workspace" $
      let prop (ArbitraryUniverse universe) = isCurrentPlayerCuttingForest ==> hasMostRecentWorkerCuttingForest
            where isCurrentPlayerCuttingForest = has (players . traverse . playerStatus . filtered (==CuttingForest)) universe
                  hasMostRecentWorkerCuttingForest = fromMaybe False $ do
                    currentPlayerData <- universe ^? (players . traverse . filtered (has (playerStatus . filtered (==CuttingForest))))
                    mostRecentWorkerId <- currentPlayerData ^? (mostRecentWorker . traverse)
                    recentWorkerWorkplace <- currentPlayerData ^? (workers . ix mostRecentWorkerId . currentWorkplace . traverse)
                    workplaceData <- universe ^? (availableWorkplaces . ix recentWorkerWorkplace)
                    return $ case workplaceData of
                               CutForest _ -> True
      in prop,
    testProperty "If a player is in MovingWorker state, then he has a free worker" $
      let prop (ArbitraryUniverse universe) = isCurrentPlayerMovingWorker ==> hasFreeWorker
            where isCurrentPlayerMovingWorker = has (players . traverse . playerStatus . filtered (==MovingWorker)) universe
                  hasFreeWorker = fromMaybe False $ do
                    currentPlayerData <- universe ^? (players . traverse . filtered (has (playerStatus . filtered (==MovingWorker))))
                    return $ has (workers . traverse . currentWorkplace . filtered (isNothing)) currentPlayerData
      in prop,
    testProperty "WorkerIds are unique" $
      let prop (ArbitraryUniverse universe) = nub workerIds == workerIds
            where workerIds = toListOf (players . traverse . workers . to keys . traverse) universe
      in prop,
    testProperty "WorkerIds are positive" $
      let isWorkerIdPositive (WorkerId number) = number > 0
          prop (ArbitraryUniverse universe) = allOf (players . traverse . workers . to keys . traverse) isWorkerIdPositive universe
      in prop,
    testProperty "WorkplaceIds are unique" $
      let prop (ArbitraryUniverse universe) = nub workplaceIds == workplaceIds
            where workplaceIds = toListOf (availableWorkplaces . to keys . traverse) universe
      in prop,
    testProperty "WorkplaceIds are positive" $
      let isWorkplaceIdPositive (WorkplaceId number) = number > 0
          prop (ArbitraryUniverse universe) = allOf (availableWorkplaces . to keys . traverse) isWorkplaceIdPositive universe
      in prop,
    testProperty "Start working with invalid worker causes error" $
      let prop (ArbitraryUniverse universe) = emptyWorkplaceAvailable ==> isLeft $ startWorking (WorkerId (-5)) emptyWorkplace universe
            where emptyWorkplaceAvailable = not . null $ findEmptyWorkplaces universe
                  emptyWorkplace = head $ findEmptyWorkplaces universe
      in prop,
    testProperty "Start working in invalid workplace causes error" $
      let prop (ArbitraryUniverse universe) = movableWorkerAvailable ==> isLeft $ startWorking workerToMove (WorkplaceId (-5)) universe
            where movableWorkerAvailable = not . null $ findWorkersToMove universe
                  workerToMove = head $ findWorkersToMove universe
      in prop
  ]

findEmptyWorkplaces :: Universe -> [WorkplaceId]
findEmptyWorkplaces universe = availableWorkplaceIds \\ (findOccupiedWorkplaces universe)
  where availableWorkplaceIds = toListOf (availableWorkplaces . to keys . traverse) universe

findOccupiedWorkplaces :: Universe -> [WorkplaceId]
findOccupiedWorkplaces = toListOf (players . traverse . workers . traverse . currentWorkplace . traverse)

findWorkersToMove :: Universe -> [WorkerId]
findWorkersToMove universe =
  toListOf (players
            . traverse
            . filtered (has $ playerStatus . filtered (==MovingWorker))
            . workers
            . to toList
            . traverse
            . filtered (has $ _2 . currentWorkplace . filtered isNothing)
            . _1) universe

allPlayersWaiting :: Universe -> Bool
allPlayersWaiting = allOf (players . traverse . playerStatus) (==Waiting)