module RulesProperties where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Map (keys, fromList, (!), elems)
import qualified Data.Map as M
import Data.List ((\\), intersect)
import Data.Maybe (maybeToList, isNothing, fromMaybe, fromJust, listToMaybe, isJust)
import Control.Monad (guard, join, liftM2)
import Data.List.Split (chunksOf)
import Data.AdditiveGroup
import qualified Data.Set as S
import Text.Show.Pretty (ppShow)

import Generators
import Rules

rulesPropertiesTests :: TestTree
rulesPropertiesTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Rules properties" [
    testProperty "Starting working assigns worker" $
      let prop (ArbitraryUniverse universe) = hasEmptyWorkplace && hasWorkerToMove ==>
            forAll (elements $ findEmptyWorkplaces universe) $ \workplaceId ->
            forAll (elements $ findWorkersToMove universe) $ \workerId ->
            getWorkplaces universe ! workplaceId /= WorkerNeed || currentPlayerCanBuildRoom universe || currentPlayerHasFreeRoom universe ==>
            rightProp $ do
              updatedUniverse <- startWorking workerId workplaceId universe
              return $ Just workplaceId == getWorkerWorkplace updatedUniverse workerId
            where hasEmptyWorkplace = not . null $ findEmptyWorkplaces universe
                  hasWorkerToMove = not . null $ findWorkersToMove universe
      in prop,
    testGroup "Starting working sets status" $
      let prop workplaceFunc precondition playerStatusFunc (ArbitraryUniverse universe) = hasEmptyWorkplace && hasWorkerToMove && precondition universe ==>
            forAll (elements $ workplaceFunc universe) $ \workplaceId ->
            forAll (elements $ findWorkersToMove universe) $ \workerId ->
            rightProp $ do
              updatedUniverse <- startWorking workerId workplaceId universe
              return $ (getPlayerStatus updatedUniverse <$> currentPlayerId) == Just (playerStatusFunc workplaceId)
            where hasEmptyWorkplace = not . null $ workplaceFunc universe
                  hasWorkerToMove = not . null $ findWorkersToMove universe
                  currentPlayerId = getCurrentPlayer universe
      in [
        testProperty "Cutting forest" $ prop findEmptyCutForestWorkplaces (const True) (const CuttingForest),
        testProperty "Digging passage" $ prop findEmptyDigPassageWorkplaces (const True) (const DiggingPassage),
        testProperty "Digging cave" $ prop findEmptyDigCaveWorkplaces (const True) (const (MakingDecision CaveOrPassageDecision)),
        testProperty "Child desire" $ prop findEmptyWorkerNeedWorkplaces (liftM2 (||) currentPlayerHasFreeRoom currentPlayerCanBuildRoom) (MakingDecision . WorkerNeedDecision)
      ],
    testProperty "Finishing turn unassigns all workers" $
      let prop (ArbitraryUniverse universe) = allPlayersWaiting universe ==> rightProp $ do
            updatedUniverse <- finishTurn universe
            let workerFree = (== Nothing) . getWorkerWorkplace updatedUniverse
                workers = [wId | plId <- getPlayers updatedUniverse, wId <- getWorkers updatedUniverse plId]
            return $ all workerFree workers
      in prop,
    testProperty "Finishing turn is not possible without all players waiting" $
      let prop (ArbitraryUniverse universe) = not (allPlayersWaiting universe) ==> leftProp $ finishTurn universe
      in prop,
    testProperty "Moving same player twice causes an error" $
      let prop (ArbitraryUniverse universe) = (length playersAbleToMove >= 2) && (length workersToMove >= 2) && (length workplaces >= 2) ==> leftProp $ do
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
    testGroup "Next player moves worker" $
      let checkResultingUniverse nextPlayerId resultUniverse = case nextPlayerId of
            Just nextId -> getPlayerStatus resultUniverse nextId == MovingWorker
            Nothing -> null [wId | pId <- getPlayers resultUniverse, wId <- getWorkers resultUniverse pId, getWorkerWorkplace resultUniverse wId == Nothing]
          coverNextPlayer nextPlayerId = cover (isJust nextPlayerId) 20 "Next player exists" . cover (isNothing nextPlayerId) 20 "No next player exists"
          selectPositionProp positionsFunc (ArbitraryUniverse universe) = coverNextPlayer nextPlayerId $ locations /= [] && currentPlayerHasValidOccupants universe
            ==> forAll (elements locations) $ \(pos, dir) ->
            rightProp $ do
              universeAfterSelect <- selectPosition pos dir universe
              return $ checkResultingUniverse nextPlayerId universeAfterSelect
            where nextPlayerId = nextPlayerToMoveWorker universe
                  locations = positionsFunc universe
          chooseChildProp (ArbitraryUniverse universe) = coverNextPlayer nextPlayerId $
            fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) && currentPlayerHasValidOccupants universe &&
            currentPlayerHasFreeRoom universe
            ==> rightProp $ do
              universeAfterChoose <- chooseOption (WorkerNeedOption HireWorker) universe
              return $ checkResultingUniverse nextPlayerId universeAfterChoose
            where nextPlayerId = nextPlayerToMoveWorker universe
          chooseNoDiggingProp (ArbitraryUniverse universe) = coverNextPlayer nextPlayerId $
            (getPlayerStatus universe <$> getCurrentPlayer universe) == Just (MakingDecision CaveOrPassageDecision) && currentPlayerHasValidOccupants universe ==>
            rightProp $ do
              universeAfterChoose <- chooseOption (CaveOrPassageOption NoDigging) universe
              return $ checkResultingUniverse nextPlayerId universeAfterChoose
            where nextPlayerId = nextPlayerToMoveWorker universe
      in [
        testProperty "After cutting forest" $ selectPositionProp currentPlayerCutForestLocations,
        testProperty "After digging passage" $ selectPositionProp currentPlayerDigPassageLocations,
        testProperty "After digging cave" $ selectPositionProp currentPlayerDigCaveLocations,
        testProperty "After building a room" $ selectPositionProp currentPlayerBuildLivingRoomLocations,
        testProperty "After choosing create child" $ chooseChildProp,
        testProperty "After choosing no digging" $ chooseNoDiggingProp
      ],
    testProperty "Next player is first player after finishing turn" $
      let prop (ArbitraryUniverse universe) = allPlayersWaiting universe ==> firstPlayerTurnAfterFinish
            where firstPlayerTurnAfterFinish = rightProp $ do
                    nextUniverse <- finishTurn universe
                    return $ getPlayerStatus nextUniverse (head $ getPlayers nextUniverse) == MovingWorker
      in prop,
    testGroup "OccupantsInvalid status" $
      let prop positionFunc (ArbitraryUniverse universe) = positions /= [] && not (currentPlayerHasValidOccupants universe) ==>
            forAll (elements positions) $ \(pos, dir) ->
            rightProp $ do
              nextUniverse <- selectPosition pos dir universe
              let currentPlayerId = getCurrentPlayer universe
                  nextStatus = getPlayerStatus nextUniverse <$> currentPlayerId
              return $ nextStatus == Just OccupantsInvalid
            where positions = positionFunc universe
          chooseChildProp (ArbitraryUniverse universe) =
            fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) && not (currentPlayerHasValidOccupants universe) &&
            currentPlayerHasFreeRoom universe
            ==> rightProp $ do
              universeAfterChoose <- chooseOption (WorkerNeedOption HireWorker) universe
              let playerStatus = (getPlayerStatus universeAfterChoose <$> getCurrentPlayer universe)
              return $ counterexample (show $ getOccupantErrors universeAfterChoose <$> getCurrentPlayer universe) $ playerStatus === Just OccupantsInvalid
          chooseNoDiggingProp (ArbitraryUniverse universe) =
            (getPlayerStatus universe <$> getCurrentPlayer universe) == Just (MakingDecision CaveOrPassageDecision) && not (currentPlayerHasValidOccupants universe) ==>
            rightProp $ do
              universeAfterChoose <- chooseOption (CaveOrPassageOption NoDigging) universe
              return $ (getPlayerStatus universeAfterChoose <$> getCurrentPlayer universe) === Just OccupantsInvalid
      in [
        testProperty "Cutting a forest" $ prop currentPlayerCutForestLocations,
        testProperty "Digging a passage" $ prop currentPlayerDigPassageLocations,
        testProperty "Digging a cave" $ prop currentPlayerDigCaveLocations,
        testProperty "Building a room" $ prop currentPlayerBuildLivingRoomLocations,
        testProperty "Choosing create child" $ chooseChildProp,
        testProperty "Choosing no digging" $ chooseNoDiggingProp
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
              leftProp $ startWorking workerId workplaceId universe
            where hasOtherStatus plId = getPlayerStatus universe plId /= MovingWorker
                  getMovableWorkers plId = [wId | wId <- getWorkers universe plId, getWorkerWorkplace universe wId == Nothing]
      in prop,
    testGroup "Selecting position alters building space" $
      let prop positionFunc buildingConstructors (ArbitraryUniverse universe) = positionFunc universe /= [] ==>
            forAll (elements $ positionFunc universe) $ \(pos, dir) ->
            rightProp $ do
              nextUniverse <- selectPosition pos dir universe
              let buildings = zipWith id buildingConstructors [pos, pos ^+^ directionAddition dir]
                  buildingSpace = getBuildingSpace nextUniverse currentPlayerId
              return $ counterexample ("Expected: " ++ show buildings ++ "\n Building Space: "++ show buildingSpace) $ intersect buildings buildingSpace == buildings
            where currentPlayerId = fromJust $ getCurrentPlayer universe
      in [
        testProperty "Cutting forest" $ prop currentPlayerCutForestLocations [Grass, Field],
        testProperty "Digging passage" $ prop currentPlayerDigPassageLocations [Cave, Passage],
        testProperty "Digging cave" $ prop currentPlayerDigCaveLocations [Cave, Cave],
        testProperty "Building living room" $ prop currentPlayerBuildLivingRoomLocations [LivingRoom]
      ],
    testGroup "Selecting a position when not supposed to fails" $
      let prop playerStatuses positionsFunc (ArbitraryUniverse universe) =
            forAll (elements $ playersToChooseFrom) $ \playerId ->
            (getPlayerStatus universe <$> getCurrentPlayer universe) `notElem` (Just <$> playerStatuses)
              && positionsFunc universe playerId /= [] ==>
            forAll (elements $ positionsFunc universe playerId) $ \(pos, dir) ->
            leftProp $ selectPosition pos dir universe
            where playersToChooseFrom = fromMaybe (getPlayers universe) $ return <$> getCurrentPlayer universe
      in [
        testProperty "Cutting Forest" $ prop [CuttingForest] availableForestPositions,
        testProperty "Digging" $ prop [DiggingPassage, DiggingCave] availableRockPositions,
        testProperty "Building room" $ prop [BuildingLivingRoom] availableSingleCavePositions
      ],
    testGroup "Selecting a wrong position fails" $
      let prop playerStatus positionsFunc (ArbitraryUniverse universe) = currentPlayerCuttingForest ==>
            forAll (elements wrongPositions) $ \(pos, dir) ->
            leftProp $ selectPosition pos dir universe
            where currentPlayerCuttingForest = (getPlayerStatus universe <$> getCurrentPlayer universe) == Just playerStatus
                  wrongPositions = S.toList $ S.fromList [((x, y), dir) | x <- [-1..6], y <- [-1..4], dir <- allDirections]
                    S.\\ S.fromList (positionsFunc universe (fromJust $ getCurrentPlayer universe))
      in [
        testProperty "Cutting Forest" $ prop CuttingForest availableForestPositions,
        testProperty "Digging Passage" $ prop DiggingPassage availableRockPositions,
        testProperty "Digging Cave" $ prop DiggingCave availableRockPositions,
        testProperty "Building a Living Room" $ prop BuildingLivingRoom availableSingleCavePositions
      ],
    testProperty "After finishing turn workplace resources are added" $
      let prop (ArbitraryUniverse universe) = allPlayersWaiting universe ==> rightProp $ do
            nextUniverse <- finishTurn universe
            let originalWorkplaces = getWorkplaces universe
                newWorkplaces = getWorkplaces nextUniverse
                isWorkplaceId workplaceId = areWorkplaceDataOk (originalWorkplaces ! workplaceId) (newWorkplaces ! workplaceId)
                areWorkplaceDataOk (CutForest orig) (CutForest new) = if orig == 0 then new == 3 else new == orig + 1
                areWorkplaceDataOk (DigPassage orig) (DigPassage new) = new == orig + 1
                areWorkplaceDataOk (DigCave orig) (DigCave new) = new == orig + 1
                areWorkplaceDataOk WorkerNeed WorkerNeed = True
                areWorkplaceDataOk _ _ = False
            return $ all isWorkplaceId (keys originalWorkplaces)
      in prop,
    testProperty "Canceling selection starts next worker" $
      let prop (ArbitraryUniverse universe) = currentPlayerSelectingPosition && nextPlayerHasWorker ==> rightProp $ do
            nextUniverse <- cancelSelection universe
            return $ if currentPlayerId == nextPlayerId
              then (getPlayerStatus nextUniverse <$> currentPlayerId) == Just MovingWorker
              else (getPlayerStatus nextUniverse <$> currentPlayerId) == Just Waiting && (getPlayerStatus nextUniverse <$> nextPlayerId) == Just MovingWorker
            where currentPlayerSelectingPosition = (getPlayerStatus universe <$> getCurrentPlayer universe) `elem` (Just <$> cancellableSelectingPositionStatuses)
                  currentPlayerId = getCurrentPlayer universe
                  nextPlayerId = head . tail $ (dropWhile (/=currentPlayerId) (Just <$> cycle (getPlayers universe)))
                  nextPlayerHasWorker = any (isNothing . getWorkerWorkplace universe) $ (join . maybeToList) $ getWorkers universe <$> nextPlayerId
      in prop,
    testProperty "Canceling selection doesn't change workers" $
      let prop (ArbitraryUniverse universe) = currentPlayerSelectingPosition ==> rightProp $ do
            nextUniverse <- cancelSelection universe
            let workerPositions u = [getWorkerWorkplace u wId | pId <- getPlayers u, wId <- getWorkers u pId]
            return $ workerPositions universe == workerPositions nextUniverse
            where currentPlayerSelectingPosition = (getPlayerStatus universe <$> getCurrentPlayer universe) `elem` (Just <$> cancellableSelectingPositionStatuses)
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
            rightProp $ do
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
            rightProp $ do
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
    testProperty "Having a worker outside of room causes error" $
      let prop (ArbitraryUniverse universe) = playersWithNoOccupantErrors /= [] ==>
            forAll (elements $ playersWithNoOccupantErrors) $ \playerId ->
            forAll (elements $ occupantsToMove playerId) $ \occupant ->
            forAll (elements $ destinationPositions playerId) $ \destinationPosition ->
            rightProp $ do
              let origOccupants = originalOccupants playerId
                  withRemovedOccupant = M.map (filter (/= occupant)) origOccupants
                  withAddedOccupant = M.alter (Just . (occupant:) . fromMaybe []) destinationPosition withRemovedOccupant
              nextUniverse <- alterOccupants playerId withAddedOccupant universe
              let errors = getOccupantErrors nextUniverse playerId
              return $ any ((==destinationPosition) . snd) errors
            where playersWithNoOccupantErrors = [plId | plId <- getPlayers universe, getOccupantErrors universe plId == []]
                  originalOccupants playerId = getBuildingOccupants universe playerId
                  occupantsToMove playerId = join $ elems $ originalOccupants playerId
                  isPositionInvalid playerId pos = intersect [InitialRoom pos, LivingRoom pos] (getBuildingSpace universe playerId) == []
                  destinationPositions playerId = filter (isPositionInvalid playerId) availableBuildingPositions
      in prop,
    testProperty "Having a worker without a room causes error" $
      let prop (ArbitraryUniverse universe) = playersWithNoOccupantErrors /= [] ==>
            forAll (elements $ playersWithNoOccupantErrors) $ \playerId ->
            forAll (elements $ occupantsToMove playerId) $ \occupant ->
            rightProp $ do
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
            rightProp $ do
              let newOccupants = fromList $ zip [(3, 2), (3, 3)] (chunksOf 2 (originalOccupants playerId ++ [occupant]))
              nextUniverse <- alterOccupants playerId newOccupants universe
              let errors = getOccupantErrors nextUniverse playerId
              return $ not $ null errors
            where playersWithFreeBuilding = [plId | plId <- getPlayers universe, length (getWorkers universe plId) <= 3]
                  originalOccupants playerId = join $ elems $ getBuildingOccupants universe playerId
      in prop,
    testProperty "Having occupant from different player causes error" $
      let prop (ArbitraryUniverse universe) = playersWithFreeBuilding /= [] ==>
            forAll (elements $ playersWithFreeBuilding) $ \playerId ->
            forAll (elements $ getPlayers universe \\ [playerId]) $ \otherPlayerId ->
            forAll (elements $ originalOccupants otherPlayerId) $ \occupant ->
            rightProp $ do
              let newOccupants = fromList $ zip [(3, 2), (3, 3)] (chunksOf 2 (originalOccupants playerId ++ [occupant]))
              nextUniverse <- alterOccupants playerId newOccupants universe
              let errors = getOccupantErrors nextUniverse playerId
              return $ not $ null errors
            where playersWithFreeBuilding = [plId | plId <- getPlayers universe, length (getWorkers universe plId) <= 3]
                  originalOccupants playerId = join $ elems $ getBuildingOccupants universe playerId
      in prop,
    testProperty "Canceling selection is not possible when building living room" $
      let prop (ArbitraryUniverse universe) = (getPlayerStatus universe <$> getCurrentPlayer universe) == Just BuildingLivingRoom ==>
            leftProp $ do
              cancelSelection universe
      in prop,
    testProperty "Choosing build room changes state to BuildingLivingRoom" $
      let prop (ArbitraryUniverse universe) =
            fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) &&
              availableSingleCavePositions universe (fromJust $ getCurrentPlayer universe) /= [] && currentPlayerHasEnoughResourcesForLivingRoom universe ==>
            rightProp $ do
              nextUniverse <- chooseOption (WorkerNeedOption BuildRoom) universe
              return $ getPlayerStatus nextUniverse (fromJust $ getCurrentPlayer universe) == BuildingLivingRoom
      in prop,
    testProperty "Choosing build room subtracts resources" $
      let prop (ArbitraryUniverse universe) =
            fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) &&
              availableSingleCavePositions universe (fromJust $ getCurrentPlayer universe) /= [] && currentPlayerHasEnoughResourcesForLivingRoom universe ==>
            rightProp $ do
              nextUniverse <- chooseOption (WorkerNeedOption BuildRoom) universe
              let currentPlayerId = fromJust $ getCurrentPlayer universe
                  origResources = getPlayerResources universe currentPlayerId
                  newResources = getPlayerResources nextUniverse currentPlayerId
              return $ getWoodAmount newResources == getWoodAmount origResources - 4 && getStoneAmount newResources == getStoneAmount origResources - 3
      in prop,
    testProperty "Choosing build room fails when there are no caves available" $
      let prop (ArbitraryUniverse universe) =
            fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) &&
              availableSingleCavePositions universe (fromJust $ getCurrentPlayer universe) == [] && currentPlayerHasEnoughResourcesForLivingRoom universe==>
            leftProp $ chooseOption (WorkerNeedOption BuildRoom) universe
      in prop,
    testProperty "Choosing build room fails when there aren't enough resources" $
      let prop (ArbitraryUniverse universe) =
            fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) &&
              availableSingleCavePositions universe (fromJust $ getCurrentPlayer universe) /= [] && not (currentPlayerHasEnoughResourcesForLivingRoom universe) ==>
            leftProp $ chooseOption (WorkerNeedOption BuildRoom) universe
      in prop,
    testProperty "Choosing make child fails when there is no room available" $
      let prop (ArbitraryUniverse universe) =
            (not $ currentPlayerHasFreeRoom universe) && fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) ==>
            leftProp $ chooseOption (WorkerNeedOption HireWorker) universe
      in prop,
    testProperty "Choosing make child creates a new worker" $
      let prop (ArbitraryUniverse universe) =
            currentPlayerHasFreeRoom universe && fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) ==>
            rightProp $ do
              nextUniverse <- chooseOption (WorkerNeedOption HireWorker) universe
              let currentPlayerId = fromJust $ getCurrentPlayer universe
                  currentPlayerOrigWorkers = getWorkers universe currentPlayerId
                  currentPlayerNewWorkers = getWorkers nextUniverse currentPlayerId
                  allWorkerIds = [wId | plId <- getPlayers nextUniverse, wId <- getWorkers nextUniverse plId]
              return $ length currentPlayerNewWorkers == length currentPlayerOrigWorkers + 1 && length allWorkerIds == S.size (S.fromList allWorkerIds)
      in prop,
    testProperty "Choosing make child keeps occupants valid" $
      let prop (ArbitraryUniverse universe) =
            currentPlayerHasFreeRoom universe && fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) &&
            (getOccupantErrors universe <$> getCurrentPlayer universe) == Just [] ==>
            rightProp $ do
              nextUniverse <- chooseOption (WorkerNeedOption HireWorker) universe
              let currentPlayerId = fromJust $ getCurrentPlayer universe
              return $ getOccupantErrors nextUniverse currentPlayerId == []
      in prop,
    testProperty "Choosing cave changes state to DiggingCave" $
      let prop (ArbitraryUniverse universe) =
            (getPlayerStatus universe <$> getCurrentPlayer universe) == Just (MakingDecision CaveOrPassageDecision) ==>
            rightProp $ do
              nextUniverse <- chooseOption (CaveOrPassageOption ChooseCave) universe
              return $ getPlayerStatus nextUniverse (fromJust $ getCurrentPlayer universe) == DiggingCave
      in prop,
    testProperty "Choosing passage changes state to DiggingPassage" $
      let prop (ArbitraryUniverse universe) =
            (getPlayerStatus universe <$> getCurrentPlayer universe) == Just (MakingDecision CaveOrPassageDecision) ==>
            rightProp $ do
              nextUniverse <- chooseOption (CaveOrPassageOption ChoosePassage) universe
              return $ getPlayerStatus nextUniverse (fromJust $ getCurrentPlayer universe) == DiggingPassage
      in prop
  ]

currentPlayerHasEnoughResourcesForLivingRoom :: Universe -> Bool
currentPlayerHasEnoughResourcesForLivingRoom universe = fromMaybe False $ do
  currentPlayerId <- getCurrentPlayer universe
  let resources = getPlayerResources universe currentPlayerId
  return $ getWoodAmount resources >= 4 && getStoneAmount resources >= 3

rightProp :: Testable a => Either String a -> Property
rightProp result = case result of
  Left msg -> counterexample ("Should be successful but is: " ++ msg) False
  Right prop -> property prop

leftProp :: Show a => Either String a -> Property
leftProp result = case result of
  Left _ -> property True
  Right x -> counterexample ("Expected error but got: " ++ ppShow x) False

nextPlayerToMoveWorker :: Universe -> Maybe PlayerId
nextPlayerToMoveWorker universe = do
  currentPlayerId <- getCurrentPlayer universe
  let furtherPlayerIds = tail $ dropWhile (/= currentPlayerId) $ getPlayers universe ++ getPlayers universe
      playersWithFreeWorkers = [plId | plId <- furtherPlayerIds, any (isNothing . getWorkerWorkplace universe) (getWorkers universe plId)]
  listToMaybe playersWithFreeWorkers

cancellableSelectingPositionStatuses :: [PlayerStatus]
cancellableSelectingPositionStatuses = [CuttingForest, DiggingPassage, DiggingCave]

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

currentPlayerBuildLivingRoomLocations :: Universe -> [(Position, Direction)]
currentPlayerBuildLivingRoomLocations = currentPlayerSpecificLocations BuildingLivingRoom availableSingleCavePositions

availableForestPositions :: Universe -> PlayerId -> [(Position, Direction)]
availableForestPositions = availableSpecificPositions isCuttable isDevelopedOutside False
  where isCuttable buildingSpace pos = Forest pos `elem` buildingSpace
        isDevelopedOutside buildingSpace pos = not $ null $ intersect [Field pos, Grass pos, InitialRoom pos] buildingSpace

availableRockPositions :: Universe -> PlayerId -> [(Position, Direction)]
availableRockPositions = availableSpecificPositions isDiggable isDevelopedInside False
  where isDiggable buildingSpace pos = Rock pos `elem` buildingSpace
        isDevelopedInside buildingSpace pos = not $ null $ intersect [InitialRoom pos, Cave pos, Passage pos] buildingSpace

availableSingleCavePositions :: Universe -> PlayerId -> [(Position, Direction)]
availableSingleCavePositions = availableSpecificPositions isBuildable (const $ const True) True
  where isBuildable buildingSpace pos = Cave pos `elem` buildingSpace

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

findEmptyWorkerNeedWorkplaces :: Universe -> [WorkplaceId]
findEmptyWorkerNeedWorkplaces = findEmptySpecificWorkplaces (==WorkerNeed)

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

currentPlayerHasFreeRoom :: Universe -> Bool
currentPlayerHasFreeRoom universe = fromMaybe False $ do
  currentPlayerId <- getCurrentPlayer universe
  let buildingSpace = getBuildingSpace universe currentPlayerId
      buildingCount (LivingRoom _) = 1
      buildingCount (InitialRoom _) = 2
      buildingCount _ = 0
      totalRoom = sum $ buildingCount <$> buildingSpace
  return (totalRoom > (length $ getWorkers universe currentPlayerId))

currentPlayerCanBuildRoom :: Universe -> Bool
currentPlayerCanBuildRoom universe = (not $ null $ join $ maybeToList $ availableSingleCavePositions universe <$> getCurrentPlayer universe) &&
  currentPlayerHasEnoughResourcesForLivingRoom universe

isChoosingWorkerNeed :: PlayerStatus -> Bool
isChoosingWorkerNeed (MakingDecision (WorkerNeedDecision _)) = True
isChoosingWorkerNeed _ = False
