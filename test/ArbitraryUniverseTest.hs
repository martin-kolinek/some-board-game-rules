module ArbitraryUniverseTest where

import Control.Lens hiding (universe, elements)
import Test.Tasty.QuickCheck
import Test.Tasty
import Data.List (nub, (\\), group, sort)
import Data.Map (toList, keys, (!))
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
import Universe.Player hiding (currentPlayerData)

arbitraryUniverseTests :: TestTree
arbitraryUniverseTests = localOption (QuickCheckMaxRatio 100) $ testGroup "Arbitrary universe tests" [
    testProperty "Generated buildings don't overlap" $
      let noBuildingsOverlap buildings = positions == (nub positions)
            where positions = [pos | building <- buildings, pos <- buildingPositions building]
      in forAll (generateBuildingSpace 0) noBuildingsOverlap,
    testProperty "Generate buildings have all positions" $
      let buildingsHaveAllPositions buildings = S.fromList positions == S.fromList [(x, y)| x <- [0..5], y <- [0..3]]
            where positions = [pos | building <- buildings, pos <- buildingPositions building]
      in forAll (generateBuildingSpace 0) buildingsHaveAllPositions,
    testProperty "Universe has at most 1 active player" $
      let isActive playerData = playerData ^. playerStatus /= Waiting
          prop (ArbitraryUniverse universe) = (lengthOf (players . traverse . filtered isActive) universe) <= 1
      in prop,
    testProperty "No two workers are in the same workplace" $
      let prop (ArbitraryUniverse universe) = all verifyGroup sortedGroups
            where workplaces = toListOf (players . traverse . workers . traverse . currentWorkplace . traverse) universe
                  sortedGroups = group $ sort workplaces
                  verifyGroup grp = length grp <= (maxWorkers $ (universe ^. availableWorkplaces) ! head grp)
                  maxWorkers WorkerNeed = 2
                  maxWorkers _ = 1
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
    testProperty "If a player is in CuttingForest state then he has a worker which is in cut forest workspace" $
      let prop (ArbitraryUniverse universe) = isCurrentPlayerCuttingForest ==> hasWorkerCuttingForest
            where isCurrentPlayerCuttingForest = has cuttingForestPlayerTraversal universe
                  hasWorkerCuttingForest = has (cuttingForestPlayerTraversal .
                                                workers .
                                                traverse .
                                                currentWorkplace .
                                                traverse .
                                                to (workplaces !) .
                                                filtered isCutForest) universe
                  isCutForest (CutForest _) = True
                  isCutForest _ = False
                  workplaces = universe ^. availableWorkplaces
                  cuttingForestPlayerTraversal =
                    players . traverse . filtered (notNullOf (playerStatus . filtered (==CuttingForest)))
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
      in prop,
    testProperty "Caves exist" $
      let prop (ArbitraryUniverse universe) =
            has (players . traverse . buildingSpace . to getBuildings . traverse . filtered isCave) universe ==> True
      in prop,
    testProperty "Passages exist" $
      let prop (ArbitraryUniverse universe) =
            has (players . traverse . buildingSpace . to getBuildings . traverse . filtered isPassage) universe ==> True
            where isPassage (Passage _) = True
                  isPassage _ = False
      in prop,
    testProperty "Living rooms exist" $
      let prop (ArbitraryUniverse universe) =
            has (players . traverse . buildingSpace . to getBuildings . traverse . filtered isLivingRoom) universe ==> True
            where isLivingRoom (LivingRoom _) = True
                  isLivingRoom _ = False
      in prop,
    testProperty "Current player can be the last player" $
      let prop (ArbitraryUniverse universe) =
            isCurrentPlayerLast ==> True
            where isCurrentPlayerLast = length dropped > 1
                  dropped = dropWhile (/= currentPlayerId) (Just <$> (keys $ universe ^. (players)))
                  currentPlayerId = universe ^? (players . to toList . traverse . filtered (has $ _2 . playerStatus . filtered (== CuttingForest)) . _1)
      in prop,
    testProperty "ChoosingWorkerNeedOption workplace is correct" $
      let prop (ArbitraryUniverse universe) =
            has currentPlayer universe ==>
            isWorkplaceWorkerNeed && isWorkplaceOccupied
            where (MakingDecision (WorkerNeedDecision workplaceId)) = fromJust $ universe ^? currentPlayer . playerStatus
                  isWorkplaceOccupied = has (currentPlayer .
                                             workers .
                                             traverse .
                                             currentWorkplace .
                                             traverse .
                                             filtered (== workplaceId)) universe
                  isWorkplaceWorkerNeed = has (availableWorkplaces . ix workplaceId . filtered (==WorkerNeed)) universe
                  currentPlayer :: Traversal' Universe PlayerData
                  currentPlayer = players . traverse . filtered (has $ playerStatus . filtered isChoosingWorkerNeed)
      in prop,
    testProperty "Current player can have valid occupants" $
      let prop (ArbitraryUniverse universe) = cover currentPlayerHasValidWorkers 30 "Valid workers" $
            getCurrentPlayer universe /= Nothing ==> True
            where currentPlayerHasValidWorkers = (getOccupantErrors universe <$> getCurrentPlayer universe) == Just []
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

isChoosingWorkerNeed :: PlayerStatus -> Bool
isChoosingWorkerNeed (MakingDecision (WorkerNeedDecision _)) = True
isChoosingWorkerNeed _ = False
