module RulesProperties where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Data.Map (keys, (!), elems, insert)
import qualified Data.Map as M
import Data.List ((\\), intersect)
import Data.Maybe (maybeToList, isNothing, fromMaybe)
import Control.Monad (guard, join, forM_)
import Data.AdditiveGroup
import Text.Show.Pretty (ppShow)
import Data.Foldable (foldl')
import Data.Monoid ((<>))

import Generators
import Rules
import TestFramework

rulesPropertiesTests :: TestTree
rulesPropertiesTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Rules properties" [
    testProperty "Starting working assigns worker" $ universeProperty $ do
        workplaceId <- pickEmptyWorkplace
        (_, workerId) <- pickWorkerToMove
        do
          workplaces <- getsUniverse getWorkplaces
          canBuildRoom <- getsUniverse currentPlayerCanBuildRoom
          hasFreeRoom <- getsUniverse currentPlayerHasFreeRoom
          pre $ workplaces ! workplaceId /= WorkerNeed || canBuildRoom || hasFreeRoom
        applyToUniverse $ startWorking workerId workplaceId
        workerWorkplace <- getsUniverse (flip getWorkerWorkplace workerId)
        assert $ workerWorkplace == Just workplaceId
    ,
    testProperty "Finishing turn unassigns all workers" $ universeProperty $ do
      pre =<< getsUniverse allPlayersWaiting
      applyToUniverse finishTurn
      updatedUniverse <- getUniverse
      let workerFree = (== Nothing) . getWorkerWorkplace updatedUniverse
          workers = [wId | plId <- getPlayers updatedUniverse, wId <- getWorkers updatedUniverse plId]
      assert $ all workerFree workers,
    testProperty "Finishing turn starts starting player" $ universeProperty $ do
      pre =<< getsUniverse allPlayersWaiting
      startingPlayer <- getsUniverse getStartingPlayer
      applyToUniverse finishTurn
      currentPlayer <- getsUniverse getCurrentPlayer
      assert $ currentPlayer == Just startingPlayer,
    testProperty "Finishing turn is not possible without all players waiting" $ universeProperty $ do
      pre =<< getsUniverse (not . allPlayersWaiting)
      currentPlayer <- getsUniverse getCurrentPlayer
      monitor (counterexample $ "Current player: " ++ show currentPlayer)
      applyToUniverse finishTurn
      shouldHaveFailed,
    testProperty "Moving same player twice causes an error" $ universeProperty $ do
      universe <- getUniverse
      let workersToMove = findWorkersToMove universe
          workplaces = findEmptyWorkplaces universe
          playersAbleToMove = [plId | plId <- getPlayers universe, any (isNothing . getWorkerWorkplace universe) (getWorkers universe plId)]
      pre $ length playersAbleToMove >= 2
      pre $ length workersToMove >= 2
      pre $ length workplaces >= 2
      applyToUniverse $ startWorking (snd $ workersToMove !! 0) (workplaces !! 0)
      applyToUniverse $ startWorking (snd $ workersToMove !! 1) (workplaces !! 1)
      shouldHaveFailed,
    testProperty "Getting workplace workers works" $ \(ArbitraryUniverse universe) ->
        (not . null) (findOccupiedWorkplaces universe) ==>
        forAll (elements $ findOccupiedWorkplaces universe) $ \workplaceId ->
        let occupants = getWorkplaceOccupants universe workplaceId
            workplaceIsNotEmpty = (not . null) occupants
            workerWorksInWorkplace workerId = getWorkerWorkplace universe workerId == Just workplaceId
            allWorkersWorkInWorkplace = all workerWorksInWorkplace occupants
        in workplaceIsNotEmpty && allWorkersWorkInWorkplace,
    testProperty "Cannot end turn while occupants are invalid" $ universeProperty $ do
      maybeCurrentPlayerId <- getsUniverse getCurrentPlayer
      currentPlayerId <- preMaybe maybeCurrentPlayerId
      universe <- getUniverse
      let occupantErrors = getOccupantErrors universe currentPlayerId
      pre $ not $ null $ occupantErrors
      forM_ (allPossibleOutcomes universe) $ \(msg, outcome) ->
        if getCurrentPlayer outcome /= Just currentPlayerId
        then do
          monitor $ counterexample ("Universe with different current player: \n " ++ msg ++ "\n" ++ ppShow outcome)
          assert False
        else return (),
    testProperty "When current player cannot do anything he has invalid occupants" $ universeProperty $ do
      currentPlayerId <- preMaybe =<< getsUniverse getCurrentPlayer
      universe <- getUniverse
      pre $ not $ isMovingWorker universe currentPlayerId
      pre $ not $ isPlantingCrops universe currentPlayerId
      pre $ not $ isSelectingPosition universe currentPlayerId
      pre $ null $ getPossibleDecisions universe currentPlayerId
      assert $ not $ null $ getOccupantErrors universe currentPlayerId,
    testProperty "When current player cannot do anything fixing occupants starts next player" $ universeProperty $ do
      currentPlayerId <- preMaybe =<< getsUniverse getCurrentPlayer
      universe <- getUniverse
      pre $ not $ isMovingWorker universe currentPlayerId
      pre $ not $ isPlantingCrops universe currentPlayerId
      pre $ not $ isSelectingPosition universe currentPlayerId
      pre $ null $ getPossibleDecisions universe currentPlayerId
      applyToUniverse $ alterOccupants currentPlayerId (createValidOccupants universe currentPlayerId)
      validateNextPlayer currentPlayerId,
    testProperty "Player not moving worker cannot move worker" $ universeProperty $ do
      players <- getsUniverse getPlayers
      playerId <- pick $ elements players
      universe <- getUniverse
      pre $ not $ isMovingWorker universe playerId
      forM_ (getWorkers universe playerId) $ \workerId ->
        forM_ (findEmptyWorkplaces universe) $ \workplaceId ->
                case startWorking workerId workplaceId universe of
                  Left _ -> return ()
                  Right u -> do
                               monitor $ counterexample ("Could move worker " ++ show workerId ++ " to " ++ show workplaceId ++ ", result: \n" ++ ppShow u)
                               assert False
      return (),
    testProperty "Reverting occupants returns original errors" $
      let prop (ArbitraryUniverse universe) =
            forAll (elements $ getPlayers universe) $ \playerId ->
            forAll (generateOccupantsForPlayer universe playerId) $ \newOccupants ->
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
          correct universe playerId = (DogOccupant <$> getDogs universe playerId) ++ (WorkerOccupant <$> getWorkers universe playerId) == getAllOccupants universe playerId
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
                  occupantsToMove playerId = filter isWorker $ join $ elems $ originalOccupants playerId
                  isWorker (WorkerOccupant _) = True
                  isWorker _ = False
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
              let newOccupants = positionOccupants (getBuildingSpace universe playerId) (originalOccupants playerId ++ [occupant])
              nextUniverse <- alterOccupants playerId newOccupants universe
              let errors = getOccupantErrors nextUniverse playerId
              return $
                counterexample ("new occupants: " ++ show newOccupants) $
                counterexample ("original occupants: " ++ show (originalOccupants playerId)) $
                not $ null errors
            where playersWithFreeBuilding = [plId | plId <- getPlayers universe, length (getWorkers universe plId) <= 3]
                  originalOccupants playerId = join $ elems $ getBuildingOccupants universe playerId
      in prop,
    testProperty "Having occupant from different player causes error" $
      let prop (ArbitraryUniverse universe) = playersWithFreeBuilding /= [] && length (getPlayers universe) > 1 ==>
            forAll (elements $ playersWithFreeBuilding) $ \playerId ->
            forAll (elements $ getPlayers universe \\ [playerId]) $ \otherPlayerId ->
            forAll (elements $ originalOccupants otherPlayerId) $ \occupant ->
            rightProp $ do
              let newOccupants = positionOccupants (getBuildingSpace universe playerId) (originalOccupants playerId ++ [occupant])
              nextUniverse <- alterOccupants playerId newOccupants universe
              let errors = getOccupantErrors nextUniverse playerId
              return $ not $ null errors
            where playersWithFreeBuilding = [plId | plId <- getPlayers universe, length (getWorkers universe plId) <= 3]
                  originalOccupants playerId = join $ elems $ getBuildingOccupants universe playerId
      in prop
  ]

allPossibleOptions :: [Options]
allPossibleOptions = (WorkerNeedOption <$> [HireWorker ..]) ++ (CaveOrPassageOption <$> [ChooseCave ..]) ++ (AnyRoomOption <$> [ChooseNoRoom ..])

allPossibleOutcomes :: Universe -> [(String, Universe)]
allPossibleOutcomes universe = let
  positionResults = [("Selected position " ++ show (x, y), selectPosition (x, y) dir universe) | x <- [0..5], y <- [0..3], dir <- allDirections]
  decisionResults = [("Chose option " ++ show option, chooseOption option universe) | option <- allPossibleOptions]
  cancelPositionResults = [("Canceled selection", cancelSelection universe)]
  finishTurnResults = [("Finished turn", finishTurn universe)]
  startWorkingReults = [("Started working " ++ show workerId ++ " in " ++ show workplaceId, startWorking workerId workplaceId universe)
                         | (_, workerId) <- findWorkersToMove universe, workplaceId <- findEmptyWorkplaces universe]
  extractRight (msg, Right res) = [(msg, res)]
  extractRight _ = []
  in join $ extractRight <$> positionResults ++ decisionResults ++ cancelPositionResults ++ finishTurnResults ++ startWorkingReults

pickAnyEmptyWorkplace :: (Universe -> [WorkplaceId]) -> UniversePropertyMonad WorkplaceId
pickAnyEmptyWorkplace workplaceSelector = do
  workplaces <- getsUniverse workplaceSelector
  pre $ not $ null $ workplaces
  pick $ elements workplaces

pickEmptyWorkplace :: UniversePropertyMonad WorkplaceId
pickEmptyWorkplace = pickAnyEmptyWorkplace findEmptyWorkplaces

pickWorkerToMove :: UniversePropertyMonad (PlayerId, WorkerId)
pickWorkerToMove = do
  workers <- getsUniverse findWorkersToMove
  pre $ not $ null $ workers
  pick $ elements workers

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

findEmptyResourceAdditionWorkplaces :: Universe -> [WorkplaceId]
findEmptyResourceAdditionWorkplaces = findEmptySpecificWorkplaces (==ResourceAddition)

findEmptyGatherWoodWorkplaces :: Universe -> [WorkplaceId]
findEmptyGatherWoodWorkplaces = findEmptySpecificWorkplaces isGatherWood
  where isGatherWood (GatherWood _) = True
        isGatherWood _ = False

findEmptyGatherFoodWorkplaces :: Universe -> [WorkplaceId]
findEmptyGatherFoodWorkplaces = findEmptySpecificWorkplaces isGatherFood
  where isGatherFood (GatherFood _) = True
        isGatherFood _ = False

findEmptyMakeStartPlayerWorkplaces :: Universe -> [WorkplaceId]
findEmptyMakeStartPlayerWorkplaces = findEmptySpecificWorkplaces isMakeStartPlayer
  where isMakeStartPlayer (MakeStartPlayer _) = True
        isMakeStartPlayer _ = False

findEmptyHouseWorkWorkplaces :: Universe -> [WorkplaceId]
findEmptyHouseWorkWorkplaces = findEmptySpecificWorkplaces (==HouseWork)

findEmptyFarmingWorkplaces :: Universe -> [WorkplaceId]
findEmptyFarmingWorkplaces = findEmptySpecificWorkplaces (== Farming)

findEmptySpecificWorkplaces :: (WorkplaceData -> Bool) -> Universe -> [WorkplaceId]
findEmptySpecificWorkplaces condition universe = (keys $ filteredWorkplaces) \\ findOccupiedWorkplaces universe
  where filteredWorkplaces = M.filter condition $ getWorkplaces universe

findEmptyWorkplaces :: Universe -> [WorkplaceId]
findEmptyWorkplaces = findEmptySpecificWorkplaces (const True)

findWorkersToMove :: Universe -> [(PlayerId, WorkerId)]
findWorkersToMove universe = do
  playerId <- getPlayers universe
  guard $ isMovingWorker universe playerId
  workerId <- getWorkers universe playerId
  guard $ getWorkerWorkplace universe workerId == Nothing
  return (playerId, workerId)

allPlayersWaiting :: Universe -> Bool
allPlayersWaiting universe = getCurrentPlayer universe == Nothing

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

createValidOccupants :: Universe -> PlayerId -> M.Map Position [BuildingOccupant]
createValidOccupants universe playerId =
  positionOccupants (getBuildingSpace universe playerId) (getAllOccupants universe playerId)

positionOccupants :: [Building] -> [BuildingOccupant] -> BuildingOccupants
positionOccupants buildings allOccupants =
  let workers = filter isWorker allOccupants
      isWorker (WorkerOccupant _) = True
      isWorker _ = False
      dogs = filter isDog allOccupants
      isDog (DogOccupant _) = True
      isDog _ = False
      accumulateWorkers (occupants, remainingWorkers) (LivingRoom pos)  = (insert pos (take 1 remainingWorkers) occupants, drop 1 remainingWorkers)
      accumulateWorkers (occupants, remainingWorkers) (InitialRoom pos) = (insert pos (take 2 remainingWorkers) occupants, drop 2 remainingWorkers)
      accumulateWorkers accumulator _ = accumulator
      (resultOccupants, nonPositionedWorkers) = foldl' accumulateWorkers (M.empty, workers) buildings
      additionalPosition = M.singleton (3, 3) nonPositionedWorkers
  in M.unionWith (<>) resultOccupants (M.singleton (0, 0) dogs <> additionalPosition)

