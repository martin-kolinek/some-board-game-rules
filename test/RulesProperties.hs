module RulesProperties where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Either (isLeft)
import Data.Map (keys, fromList, (!), elems)
import qualified Data.Map as M
import Data.List ((\\), intersect)
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
    testGroup "Starting working sets status" $
      let prop workplaceFunc playerStatus (ArbitraryUniverse universe) = hasEmptyWorkplace && hasWorkerToMove ==>
            forAll (elements $ workplaceFunc universe) $ \workplaceId ->
            forAll (elements $ findWorkersToMove universe) $ \workerId ->
            either (const False) id $ do
              updatedUniverse <- startWorking workerId workplaceId universe
              return $ (getPlayerStatus updatedUniverse <$> currentPlayerId) == Just playerStatus
            where hasEmptyWorkplace = not . null $ workplaceFunc universe
                  hasWorkerToMove = not . null $ findWorkersToMove universe
                  currentPlayerId = getCurrentPlayer universe
      in [
        testProperty "Cutting forest" $ prop findEmptyCutForestWorkplaces CuttingForest,
        testProperty "Digging passage" $ prop findEmptyDigPassageWorkplaces DiggingPassage,
        testProperty "Digging cave" $ prop findEmptyDigCaveWorkplaces DiggingCave,
        testProperty "Child desire" $ prop findEmptyChildDesireWorkplaces ChoosingChildDesireOption
      ],
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
      let prop (ArbitraryUniverse universe) = (length playersAbleToMove >= 2) && (length workersToMove >= 2) && (length workplaces >= 2) ==> isLeft $ do
            universeWithFirstMovement <- startWorking (workersToMove !! 0) (workplaces !! 0) universe
            startWorking (workersToMove !! 1) (workplaces !! 1) universeWithFirstMovement
              where workersToMove = findWorkersToMove universe
                    workplaces = findEmptyWorkplaces universe
                    playersAbleToMove = [plId | plId <- getPlayers universe, any (isNothing . getWorkerWorkplace universe) (getWorkers universe plId)]
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
    testGroup "OccupantsInvalid status" $
      let prop positionFunc (ArbitraryUniverse universe) = positions /= [] && not (currentPlayerHasValidOccupants universe) ==>
            forAll (elements positions) $ \(pos, dir) ->
            either (const False) id $ do
              nextUniverse <- selectPosition pos dir universe
              let currentPlayerId = getCurrentPlayer universe
                  nextStatus = getPlayerStatus nextUniverse <$> currentPlayerId
              return $ nextStatus == Just OccupantsInvalid
            where positions = positionFunc universe
      in [
        testProperty "Cutting a forest" $ prop currentPlayerCutForestLocations,
        testProperty "Digging a passage" $ prop currentPlayerDigPassageLocations,
        testProperty "Digging a cave" $ prop currentPlayerDigCaveLocations
      ],
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
    testGroup "Selecting position alters building space" $
      let prop positionFunc buildingConstructors (ArbitraryUniverse universe) = positionFunc universe /= [] ==>
            forAll (elements $ positionFunc universe) $ \(pos, dir) ->
            either (const False) id $ do
              nextUniverse <- selectPosition pos dir universe
              let buildings = zipWith id buildingConstructors [pos, pos ^+^ directionAddition dir]
              return $ intersect buildings (getBuildingSpace nextUniverse currentPlayerId) == buildings
            where currentPlayerId = fromJust $ getCurrentPlayer universe
      in [
        testProperty "Cutting forest" $ prop currentPlayerCutForestLocations [Grass, Field],
        testProperty "Digging passage" $ prop currentPlayerDigPassageLocations [Cave, Passage],
        testProperty "Digging cave" $ prop currentPlayerDigCaveLocations [Cave, Cave]
      ],
    testGroup "Selecting a position when not supposed to fails" $
      let prop playerStatuses positionsFunc (ArbitraryUniverse universe) =
            forAll (elements $ getPlayers universe) $ \playerId ->
            (getPlayerStatus universe <$> getCurrentPlayer universe) `notElem` (Just <$> playerStatuses)
              && positionsFunc universe playerId /= [] ==>
            forAll (elements $ positionsFunc universe playerId) $ \(pos, dir) ->
            isLeft $ selectPosition pos dir universe
      in [
        testProperty "Cutting Forest" $ prop [CuttingForest] availableForestPositions,
        testProperty "Digging" $ prop [DiggingPassage, DiggingCave] availableRockPositions
      ],
    testGroup "Selecting a wrong position fails" $
      let prop playerStatus positionsFunc (ArbitraryUniverse universe) = currentPlayerCuttingForest ==>
            forAll (elements wrongPositions) $ \(pos, dir) ->
            isLeft $ selectPosition pos dir universe
            where currentPlayerCuttingForest = (getPlayerStatus universe <$> getCurrentPlayer universe) == Just playerStatus
                  wrongPositions = S.toList $ S.fromList [((x, y), dir) | x <- [-1..6], y <- [-1..4], dir <- allDirections]
                    S.\\ S.fromList (positionsFunc universe (fromJust $ getCurrentPlayer universe))
      in [
        testProperty "Cutting Forest" $ prop CuttingForest availableForestPositions,
        testProperty "Digging Passage" $ prop DiggingPassage availableRockPositions,
        testProperty "Digging Cave" $ prop DiggingCave availableRockPositions
      ],
    testProperty "After finishing turn workplace resources are added" $
      let prop (ArbitraryUniverse universe) = allPlayersWaiting universe ==> either (const False) id $ do
            nextUniverse <- finishTurn universe
            let originalWorkplaces = getWorkplaces universe
                newWorkplaces = getWorkplaces nextUniverse
                isWorkplaceId workplaceId = areWorkplaceDataOk (originalWorkplaces ! workplaceId) (newWorkplaces ! workplaceId)
                areWorkplaceDataOk (CutForest orig) (CutForest new) = if orig == 0 then new == 3 else new == orig + 1
                areWorkplaceDataOk (DigPassage orig) (DigPassage new) = new == orig + 1
                areWorkplaceDataOk (DigCave orig) (DigCave new) = new == orig + 1
                areWorkplaceDataOk ChildDesire ChildDesire = True
                areWorkplaceDataOk _ _ = False
            return $ all isWorkplaceId (keys originalWorkplaces)
      in prop,
    testProperty "Canceling selection starts next worker" $
      let prop (ArbitraryUniverse universe) = currentPlayerSelectingPosition && nextPlayerHasWorker ==> either (const False) id $ do
            nextUniverse <- cancelSelection universe
            return $ if currentPlayerId == nextPlayerId
              then (getPlayerStatus nextUniverse <$> currentPlayerId) == Just MovingWorker
              else (getPlayerStatus nextUniverse <$> currentPlayerId) == Just Waiting && (getPlayerStatus nextUniverse <$> nextPlayerId) == Just MovingWorker
            where currentPlayerSelectingPosition = (getPlayerStatus universe <$> getCurrentPlayer universe) `elem` (Just <$> selectingPositionStatuses)
                  currentPlayerId = getCurrentPlayer universe
                  nextPlayerId = head . tail $ (dropWhile (/=currentPlayerId) (Just <$> cycle (getPlayers universe)))
                  nextPlayerHasWorker = any (isNothing . getWorkerWorkplace universe) $ (join . maybeToList) $ getWorkers universe <$> nextPlayerId
      in prop,
    testProperty "Canceling selection doesn't change workers" $
      let prop (ArbitraryUniverse universe) = currentPlayerSelectingPosition ==> either (const False) id $ do
            nextUniverse <- cancelSelection universe
            let workerPositions u = [getWorkerWorkplace u wId | pId <- getPlayers u, wId <- getWorkers u pId]
            return $ workerPositions universe == workerPositions nextUniverse
            where currentPlayerSelectingPosition = (getPlayerStatus universe <$> getCurrentPlayer universe) `elem` (Just <$> selectingPositionStatuses)
      in prop,
    testGroup "Adding resources" $
      let getWorkplaceWoodAmount (CutForest n) = n
          getWorkplaceWoodAmount _ = 0
          getWorkplaceStoneAmount (DigCave n) = n
          getWorkplaceStoneAmount (DigPassage n) = n
          getWorkplaceStoneAmount _ = 0
          prop workplacesFunc resourceFunc workplaceResourceFunc (ArbitraryUniverse universe) = findWorkersToMove universe /= [] && workplacesFunc universe /= [] ==>
            forAll (elements $ findWorkersToMove universe) $ \workerId ->
            forAll (elements $ workplacesFunc universe) $ \workplaceId ->
            either (const False) id $ do
            nextUniverse <- startWorking workerId workplaceId universe
            let playerId = head $ [pId | pId <- getPlayers universe, wId <- getWorkers universe pId, wId == workerId]
                origAmount = resourceFunc $ getPlayerResources universe playerId
                newAmount = resourceFunc $ getPlayerResources nextUniverse playerId
                origWorkplaceAmount = workplaceResourceFunc $ (getWorkplaces universe) ! workplaceId
            return $ newAmount - origAmount == origWorkplaceAmount
      in [
        testProperty "Cutting forest" $ prop findEmptyCutForestWorkplaces getWoodAmount getWorkplaceWoodAmount,
        testProperty "Digging passage" $ prop findEmptyDigPassageWorkplaces getStoneAmount getWorkplaceStoneAmount,
        testProperty "Digging cave" $ prop findEmptyDigCaveWorkplaces getStoneAmount getWorkplaceStoneAmount
      ],
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

selectingPositionStatuses :: [PlayerStatus]
selectingPositionStatuses = [CuttingForest, DiggingPassage, DiggingCave]

currentPlayerSpecificLocations :: PlayerStatus -> (Universe -> PlayerId -> [(Position, Direction)]) -> Universe -> [(Position, Direction)]
currentPlayerSpecificLocations status positionCollector universe = do
  let currentPlayerId = getCurrentPlayer universe
  cPlId <- maybeToList currentPlayerId
  let currentPlayerMatchesStatus = (getPlayerStatus universe <$> currentPlayerId) == Just status
  guard currentPlayerMatchesStatus
  positionCollector universe cPlId

currentPlayerCutForestLocations :: Universe -> [(Position, Direction)]
currentPlayerCutForestLocations = currentPlayerSpecificLocations CuttingForest availableForestPositions

currentPlayerDigPassageLocations :: Universe -> [(Position, Direction)]
currentPlayerDigPassageLocations = currentPlayerSpecificLocations DiggingPassage availableRockPositions

currentPlayerDigCaveLocations :: Universe -> [(Position, Direction)]
currentPlayerDigCaveLocations = currentPlayerSpecificLocations DiggingCave availableRockPositions

availableForestPositions :: Universe -> PlayerId -> [(Position, Direction)]
availableForestPositions = availableSpecificPositions isCuttable isDevelopedOutside
  where isCuttable buildingSpace pos = Forest pos `elem` buildingSpace
        isDevelopedOutside buildingSpace pos = not $ null $ intersect [Field pos, Grass pos, InitialRoom pos] buildingSpace

availableRockPositions :: Universe -> PlayerId -> [(Position, Direction)]
availableRockPositions = availableSpecificPositions isDiggable isDevelopedInside
  where isDiggable buildingSpace pos = Rock pos `elem` buildingSpace
        isDevelopedInside buildingSpace pos = not $ null $ intersect [InitialRoom pos, Cave pos, Passage pos] buildingSpace

availableSpecificPositions :: ([Building] -> Position -> Bool) -> ([Building] -> Position -> Bool) -> Universe -> PlayerId -> [(Position, Direction)]
availableSpecificPositions freeCondition developedCondition universe playerId = [(pos, direction) |
                             direction <- allDirections,
                             pos <- availableBuildingPositions,
                             freeCondition buildingSpace pos,
                             freeCondition buildingSpace (pos ^+^ directionAddition direction),
                             neighbourPositionsReachable [pos, pos ^+^ directionAddition direction]]
  where neighbourPositionsReachable positions = any (developedCondition buildingSpace) [pos ^+^ directionAddition dir | pos <- positions, dir <- allDirections]
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

findEmptyCutForestWorkplaces :: Universe -> [WorkplaceId]
findEmptyCutForestWorkplaces = findEmptySpecificWorkplaces isCutForest
  where isCutForest (CutForest _) = True
        isCutForest _ = False

findEmptyDigPassageWorkplaces :: Universe -> [WorkplaceId]
findEmptyDigPassageWorkplaces = findEmptySpecificWorkplaces isDigPassage
  where isDigPassage (DigPassage _) = True
        isDigPassage _ = False

findEmptyDigCaveWorkplaces :: Universe -> [WorkplaceId]
findEmptyDigCaveWorkplaces = findEmptySpecificWorkplaces isDigCave
  where isDigCave (DigCave _) = True
        isDigCave _ = False

findEmptyChildDesireWorkplaces :: Universe -> [WorkplaceId]
findEmptyChildDesireWorkplaces = findEmptySpecificWorkplaces (==ChildDesire)

findEmptySpecificWorkplaces :: (WorkplaceData -> Bool) -> Universe -> [WorkplaceId]
findEmptySpecificWorkplaces condition universe = (keys $ filteredWorkplaces) \\ findOccupiedWorkplaces universe
  where filteredWorkplaces = M.filter condition $ getWorkplaces universe

findEmptyWorkplaces :: Universe -> [WorkplaceId]
findEmptyWorkplaces = findEmptySpecificWorkplaces (const True)

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
