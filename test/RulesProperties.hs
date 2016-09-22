module RulesProperties where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Either (isLeft)
import Data.Map (keys, fromList, (!), elems)
import qualified Data.Map as M
import Data.List ((\\))
import Data.Maybe (maybeToList, isNothing, fromMaybe, fromJust)
import Control.Monad (guard, join)
import Data.List.Split (chunksOf)
import Data.AdditiveGroup
import qualified Data.Set as S

import Generators
import Rules

rulesPropertiesTests :: TestTree
rulesPropertiesTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Rules properties" [
    testProperty "Starting working assigns worker" $
      let prop (ArbitraryUniverse universe) = hasEmptyWorkplace && hasWorkerToMove ==>
            forAll (elements $ findEmptyWorkplaces universe) $ \workplaceId ->
            forAll (elements $ findWorkersToMove universe) $ \workerId ->
            either (const False) id $ do
              updatedUniverse <- startWorking workerId workplaceId universe
              return $ Just workplaceId == getWorkerWorkplace updatedUniverse workerId
            where hasEmptyWorkplace = not . null $ findEmptyWorkplaces universe
                  hasWorkerToMove = not . null $ findWorkersToMove universe
      in prop,
    testProperty "Starting working in cut forest sets CuttingForest status" $
      let prop (ArbitraryUniverse universe) = hasEmptyWorkplace && hasWorkerToMove ==>
            forAll (elements $ findEmptyWorkplaces universe) $ \workplaceId ->
            forAll (elements $ findWorkersToMove universe) $ \workerId ->
            either (const False) id $ do
              updatedUniverse <- startWorking workerId workplaceId universe
              return $ (getPlayerStatus updatedUniverse <$> currentPlayerId) == Just CuttingForest
            where hasEmptyWorkplace = not . null $ findEmptyWorkplaces universe
                  hasWorkerToMove = not . null $ findWorkersToMove universe
                  currentPlayerId = getCurrentPlayer universe
      in prop,
    testProperty "Finishing turn unassigns all workers" $
      let prop (ArbitraryUniverse universe) = allPlayersWaiting universe ==> either (const False) id $ do
            updatedUniverse <- finishTurn universe
            let workerFree = (== Nothing) . getWorkerWorkplace updatedUniverse
                workers = [wId | plId <- getPlayers updatedUniverse, wId <- getWorkers updatedUniverse plId]
            return $ all workerFree workers
      in prop,
    testProperty "Finishing turn is not possible without all players waiting" $
      let prop (ArbitraryUniverse universe) = not (allPlayersWaiting universe) ==> isLeft $ finishTurn universe
      in prop,
    testProperty "Moving same player twice causes an error" $
      let prop (ArbitraryUniverse universe) = (length workersToMove >= 2) && (length workplaces >= 2) ==> isLeft $ do
            universeWithFirstMovement <- startWorking (workersToMove !! 0) (workplaces !! 0) universe
            startWorking (workersToMove !! 1) (workplaces !! 1) universeWithFirstMovement
              where workersToMove = findWorkersToMove universe
                    workplaces = findEmptyWorkplaces universe
      in prop,
    testProperty "Getting workplace workers works" $
      let prop (ArbitraryUniverse universe) = (not . null) (findOccupiedWorkplaces universe) ==>
            forAll (elements $ findOccupiedWorkplaces universe) $ \workplaceId ->
            let occupants = getWorkplaceOccupants universe workplaceId
                workplaceIsNotEmpty = (not . null) occupants
                workerWorksInWorkplace workerId = getWorkerWorkplace universe workerId == Just workplaceId
                allWorkersWorkInWorkplace = all workerWorksInWorkplace occupants
            in workplaceIsNotEmpty && allWorkersWorkInWorkplace
      in prop,
    testProperty "Next player moves worker after cutting forest" $
      let prop (ArbitraryUniverse universe) = cutForestLocations /= [] && nextPlayerHasWorker && currentPlayerHasValidOccupants universe
            ==> forAll (elements cutForestLocations) $ \(pos, dir) ->
            either (const False) id $ do
              universeAfterSelect <- selectPosition pos dir universe
              return $ (getPlayerStatus universeAfterSelect <$> nextPlayerId) == Just MovingWorker
            where nextPlayerHasWorker = any (isNothing . getWorkerWorkplace universe) $ (join . maybeToList) $ getWorkers universe <$> nextPlayerId
                  currentPlayerId = getCurrentPlayer universe
                  nextPlayerId = (head . tail) $ dropWhile (/= currentPlayerId) $ Just <$> (cycle $ getPlayers universe)
                  cutForestLocations = currentPlayerCutForestLocations universe
      in prop,
    testProperty "No next player after moving last worker" $
      let prop (ArbitraryUniverse universe) =
            allWorkersPlayersWorking && cutForestLocations /= [] && currentPlayerHasValidOccupants universe
            ==> forAll (elements cutForestLocations) $ \(pos, dir) ->
                  either (const False) id $ do
                    nextUniverse <- selectPosition pos dir universe
                    return $ getCurrentPlayer nextUniverse == Nothing
            where allWorkersPlayersWorking =
                    null [wId | pId <- getPlayers universe, wId <- getWorkers universe pId, getWorkerWorkplace universe wId == Nothing]
                  cutForestLocations = currentPlayerCutForestLocations universe
      in prop,
    testProperty "Next player is first player after finishing turn" $
      let prop (ArbitraryUniverse universe) = allPlayersWaiting universe ==> firstPlayerTurnAfterFinish
            where firstPlayerTurnAfterFinish = either (const False) id $ do
                    nextUniverse <- finishTurn universe
                    return $ getPlayerStatus nextUniverse (head $ getPlayers nextUniverse) == MovingWorker
      in prop,
    testProperty "Cutting a forest with invalid occupants makes OccupantsInvalid status" $
      let prop (ArbitraryUniverse universe) = cutForestPositions /= [] && not (currentPlayerHasValidOccupants universe) ==>
            forAll (elements cutForestPositions) $ \(pos, dir) ->
            either (const False) id $ do
              nextUniverse <- selectPosition pos dir universe
              let currentPlayerId = getCurrentPlayer universe
                  nextStatus = getPlayerStatus nextUniverse <$> currentPlayerId
              return $ nextStatus == Just OccupantsInvalid
            where cutForestPositions = currentPlayerCutForestLocations universe
      in prop,
    testProperty "Fixing occupants in invalid occupants starts next player" $
      let prop (ArbitraryUniverse universe) = currentPlayerIsInInvalidOccupantsState ==> either (error) id $ do
            let currentPlayerId = fromJust $ getCurrentPlayer universe
                workers = getWorkers universe currentPlayerId
                workerOccupants = WorkerOccupant <$> workers
                occupants = fromList $ zip [(3, 2), (3, 3)] (chunksOf 2 workerOccupants)
            nextUniverse <- alterOccupants currentPlayerId occupants universe
            let nextPlayerMovingWorker = (getPlayerStatus nextUniverse <$> getCurrentPlayer nextUniverse) == Just MovingWorker
                currentPlayerWaiting = (getPlayerStatus nextUniverse currentPlayerId) `elem` [Waiting, MovingWorker]
            return $ (currentPlayerWaiting && nextPlayerMovingWorker) || allPlayersWaiting nextUniverse
            where currentPlayerIsInInvalidOccupantsState = (getPlayerStatus universe <$> getCurrentPlayer universe) == Just OccupantsInvalid
      in prop,
    testProperty "Player not moving worker cannot move worker" $
      let prop (ArbitraryUniverse universe) =
            forAll (elements $ getPlayers universe) $ \playerId ->
            hasOtherStatus playerId && getMovableWorkers playerId /= [] && findEmptyWorkplaces universe /= [] ==>
              forAll (elements $ getWorkers universe playerId) $ \workerId ->
              forAll (elements $ keys $ getWorkplaces universe) $ \workplaceId ->
              isLeft $ startWorking workerId workplaceId universe
            where hasOtherStatus plId = getPlayerStatus universe plId /= MovingWorker
                  getMovableWorkers plId = [wId | wId <- getWorkers universe plId, getWorkerWorkplace universe wId == Nothing]
      in prop,
    testProperty "Selecting a position when cutting forest cuts forest" $
      let prop (ArbitraryUniverse universe) = currentPlayerCuttingForest && availableForestPositions universe currentPlayerId /= [] ==>
            forAll (elements $ availableForestPositions universe currentPlayerId) $ \(pos, dir) ->
            either (const False) id $ do
              nextUniverse <- selectPosition pos dir universe
              let fieldContained = Field (pos ^+^ directionAddition dir) `elem` getBuildingSpace nextUniverse currentPlayerId
                  forestContained = Grass pos `elem` getBuildingSpace nextUniverse currentPlayerId
              return $ fieldContained && forestContained
            where currentPlayerCuttingForest = (getPlayerStatus universe <$> getCurrentPlayer universe) == Just CuttingForest
                  currentPlayerId = fromJust $ getCurrentPlayer universe
      in prop,
    testProperty "Selecting a position when not cutting forest fails" $
      let prop (ArbitraryUniverse universe) =
            forAll (elements $ getPlayers universe) $ \playerId ->
            (getPlayerStatus universe <$> getCurrentPlayer universe) /= Just CuttingForest && availableForestPositions universe playerId /= [] ==>
            forAll (elements $ availableForestPositions universe playerId) $ \(pos, dir) ->
            isLeft $ selectPosition pos dir universe
      in prop,
    testProperty "Selecting a wrong position fails" $
      let prop (ArbitraryUniverse universe) = currentPlayerCuttingForest ==>
            forAll (elements wrongPositions) $ \(pos, dir) ->
            isLeft $ selectPosition pos dir universe
            where currentPlayerCuttingForest = (getPlayerStatus universe <$> getCurrentPlayer universe) == Just CuttingForest
                  wrongPositions = S.toList $ S.fromList [((x, y), dir) | x <- [-1..6], y <- [-1..4], dir <- allDirections]
                    S.\\ S.fromList (availableForestPositions universe (fromJust $ getCurrentPlayer universe))
      in prop,
    testProperty "After finishing turn wood is added to cut forest workplaces" $
      let prop (ArbitraryUniverse universe) = allPlayersWaiting universe ==> either (const False) id $ do
            nextUniverse <- finishTurn universe
            let originalWorkplaces = getWorkplaces universe
                newWorkplaces = getWorkplaces nextUniverse
                isWorkplaceId workplaceId = areWorkplaceDataOk (originalWorkplaces ! workplaceId) (newWorkplaces ! workplaceId)
                areWorkplaceDataOk (CutForest orig) (CutForest new) = if orig == 0 then new == 3 else new == orig + 1
                areWorkplaceDataOk _ _ = False
            return $ all isWorkplaceId (keys originalWorkplaces)
      in prop,
    testProperty "Canceling selection starts next worker" $
      let prop (ArbitraryUniverse universe) = currentPlayerCuttingForest && nextPlayerHasWorker ==> either (const False) id $ do
            nextUniverse <- cancelSelection universe
            return $ if currentPlayerId == nextPlayerId
              then (getPlayerStatus nextUniverse <$> currentPlayerId) == Just MovingWorker
              else (getPlayerStatus nextUniverse <$> currentPlayerId) == Just Waiting && (getPlayerStatus nextUniverse <$> nextPlayerId) == Just MovingWorker
            where currentPlayerCuttingForest = (getPlayerStatus universe <$> getCurrentPlayer universe) == Just CuttingForest
                  currentPlayerId = getCurrentPlayer universe
                  nextPlayerId = head . tail $ (dropWhile (/=currentPlayerId) (Just <$> cycle (getPlayers universe)))
                  nextPlayerHasWorker = any (isNothing . getWorkerWorkplace universe) $ (join . maybeToList) $ getWorkers universe <$> nextPlayerId
      in prop,
    testProperty "Canceling selection doesn't change workers" $
      let prop (ArbitraryUniverse universe) = currentPlayerCuttingForest ==> either (const False) id $ do
            nextUniverse <- cancelSelection universe
            let workerPositions u = [getWorkerWorkplace u wId | pId <- getPlayers u, wId <- getWorkers u pId]
            return $ workerPositions universe == workerPositions nextUniverse
            where currentPlayerCuttingForest = (getPlayerStatus universe <$> getCurrentPlayer universe) == Just CuttingForest
      in prop,
    testProperty "Starting working in cut forest adds wood" $
      let prop (ArbitraryUniverse universe) = findWorkersToMove universe /= [] && findEmptyWorkplaces universe /= [] ==>
            forAll (elements $ findWorkersToMove universe) $ \workerId ->
            forAll (elements $ findEmptyWorkplaces universe) $ \workplaceId ->
            either (const False) id $ do
            nextUniverse <- startWorking workerId workplaceId universe
            let playerId = head $ [pId | pId <- getPlayers universe, wId <- getWorkers universe pId, wId == workerId]
                origWoodAmount = getWoodAmount $ getPlayerResources universe playerId
                newWoodAmount = getWoodAmount $ getPlayerResources nextUniverse playerId
                getWorkplaceWoodAmount (CutForest n) = n
                getWorkplaceWoodAmount _ = 0
                origWorkplaceWoodAmount = getWorkplaceWoodAmount $ (getWorkplaces universe) ! workplaceId
            return $ newWoodAmount - origWoodAmount == origWorkplaceWoodAmount
      in prop,
    testProperty "Reverting occupants returns original errors" $
      let prop (ArbitraryUniverse universe) =
            forAll (elements $ getPlayers universe) $ \playerId ->
            forAll (generateOccupants (getWorkers universe playerId)) $ \newOccupants ->
            either (const False) id $ do
              let oldOccupants = getBuildingOccupants universe playerId
              otherUniverse <- alterOccupants playerId newOccupants universe
              restoredUniverse <- alterOccupants playerId oldOccupants otherUniverse
              let originalErrors = getOccupantErrors universe playerId
                  newErrors = getOccupantErrors restoredUniverse playerId
              return $ originalErrors == newErrors
      in prop,
    testProperty "Getting all occupants returns workers" $
      let prop (ArbitraryUniverse universe) = all (correct universe) (getPlayers universe)
          correct universe playerId = (WorkerOccupant <$> getWorkers universe playerId) == getAllOccupants universe playerId
      in prop,
    testProperty "Having a worker outside of initial room causes error" $
      let prop (ArbitraryUniverse universe) = playersWithNoOccupantErrors /= [] ==>
            forAll (elements $ playersWithNoOccupantErrors) $ \playerId ->
            forAll (elements $ occupantsToMove playerId) $ \occupant ->
            forAll (elements $ destinationPositions playerId) $ \destinationPosition ->
            either (const False) id $ do
              let origOccupants = originalOccupants playerId
                  withRemovedOccupant = M.map (filter (/= occupant)) origOccupants
                  withAddedOccupant = M.alter (Just . (occupant:) . fromMaybe []) destinationPosition withRemovedOccupant
              nextUniverse <- alterOccupants playerId withAddedOccupant universe
              let errors = getOccupantErrors nextUniverse playerId
              return $ any ((==destinationPosition) . snd) errors
            where playersWithNoOccupantErrors = [plId | plId <- getPlayers universe, getOccupantErrors universe plId == []]
                  originalOccupants playerId = getBuildingOccupants universe playerId
                  occupantsToMove playerId = join $ elems $ originalOccupants playerId
                  isPositionInvalid playerId pos = not $ InitialRoom pos `elem` getBuildingSpace universe playerId
                  destinationPositions playerId = filter (isPositionInvalid playerId) availableBuildingPositions
      in prop,
    testProperty "Having a worker without a room causes error" $
      let prop (ArbitraryUniverse universe) = playersWithNoOccupantErrors /= [] ==>
            forAll (elements $ playersWithNoOccupantErrors) $ \playerId ->
            forAll (elements $ occupantsToMove playerId) $ \occupant ->
            either (const False) id $ do
              let origOccupants = originalOccupants playerId
                  withRemovedOccupant = M.map (filter (/= occupant)) origOccupants
              nextUniverse <- alterOccupants playerId withRemovedOccupant universe
              let errors = getOccupantErrors nextUniverse playerId
              return $ not $ null errors
            where playersWithNoOccupantErrors = [plId | plId <- getPlayers universe, getOccupantErrors universe plId == []]
                  originalOccupants playerId = getBuildingOccupants universe playerId
                  occupantsToMove playerId = join $ elems $ originalOccupants playerId
      in prop,
    testProperty "Having same occupant multiple times causes an error" $
      let prop (ArbitraryUniverse universe) = playersWithFreeBuilding /= [] ==>
            forAll (elements $ playersWithFreeBuilding) $ \playerId ->
            forAll (elements $ originalOccupants playerId) $ \occupant ->
            either (const False) id $ do
              let newOccupants = fromList $ zip [(3, 2), (3, 3)] (chunksOf 2 (originalOccupants playerId ++ [occupant]))
              nextUniverse <- alterOccupants playerId newOccupants universe
              let errors = getOccupantErrors nextUniverse playerId
              return $ not $ null errors
            where playersWithFreeBuilding = [plId | plId <- getPlayers universe, length (getWorkers universe plId) <= 3]
                  originalOccupants playerId = join $ elems $ getBuildingOccupants universe playerId
      in prop,
    testProperty "Having same occupant multiple times causes an error" $
      let prop (ArbitraryUniverse universe) = playersWithFreeBuilding /= [] ==>
            forAll (elements $ playersWithFreeBuilding) $ \playerId ->
            forAll (elements $ getPlayers universe \\ [playerId]) $ \otherPlayerId ->
            forAll (elements $ originalOccupants otherPlayerId) $ \occupant ->
            either (const False) id $ do
              let newOccupants = fromList $ zip [(3, 2), (3, 3)] (chunksOf 2 (originalOccupants playerId ++ [occupant]))
              nextUniverse <- alterOccupants playerId newOccupants universe
              let errors = getOccupantErrors nextUniverse playerId
              return $ not $ null errors
            where playersWithFreeBuilding = [plId | plId <- getPlayers universe, length (getWorkers universe plId) <= 3]
                  originalOccupants playerId = join $ elems $ getBuildingOccupants universe playerId
      in prop
  ]

currentPlayerCutForestLocations :: Universe -> [(Position, Direction)]
currentPlayerCutForestLocations universe = do
  let currentPlayerId = getCurrentPlayer universe
  cPlId <- maybeToList currentPlayerId
  let currentPlayerCuttingForest = (getPlayerStatus universe <$> currentPlayerId) == Just CuttingForest
  guard currentPlayerCuttingForest
  availableForestPositions universe cPlId

availableForestPositions :: Universe -> PlayerId -> [(Position, Direction)]
availableForestPositions universe playerId = [(pos, direction) |
                             direction <- allDirections,
                             pos <- availableBuildingPositions,
                             Forest pos `elem` buildingSpace,
                             Forest (pos ^+^ directionAddition direction) `elem` buildingSpace,
                             neighbourPositionsReachable [pos, pos ^+^ directionAddition direction]]
  where neighbourPositionsReachable positions = any positionDeveloped [pos ^+^ directionAddition dir | pos <- positions, dir <- allDirections]
        positionDeveloped position = Field position `elem` buildingSpace || Grass position `elem` buildingSpace || InitialRoom position `elem` buildingSpace
        buildingSpace = currentPlayerBuildingSpace universe playerId

currentPlayerBuildingSpace :: Universe -> PlayerId -> [Building]
currentPlayerBuildingSpace universe playerId = getBuildingSpace universe playerId

currentPlayerHasValidOccupants :: Universe -> Bool
currentPlayerHasValidOccupants universe = (getOccupantErrors universe <$> getCurrentPlayer universe) == Just []

findOccupiedWorkplaces :: Universe -> [WorkplaceId]
findOccupiedWorkplaces universe = do
  playerId <- getPlayers universe
  workerId <- getWorkers universe playerId
  maybeToList $ getWorkerWorkplace universe workerId

findEmptyWorkplaces :: Universe -> [WorkplaceId]
findEmptyWorkplaces universe = (keys $ getWorkplaces universe) \\ findOccupiedWorkplaces universe

findWorkersToMove :: Universe -> [WorkerId]
findWorkersToMove universe = do
  playerId <- getPlayers universe
  guard $ getPlayerStatus universe playerId == MovingWorker
  workerId <- getWorkers universe playerId
  guard $ getWorkerWorkplace universe workerId == Nothing
  return workerId

allPlayersWaiting :: Universe -> Bool
allPlayersWaiting universe = all playerWaiting players
  where playerWaiting plId = getPlayerStatus universe plId == Waiting
        players = getPlayers universe
