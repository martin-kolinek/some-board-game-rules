module RulesProperties where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Data.Map (keys, (!), elems, insert)
import qualified Data.Map as M
import Data.List ((\\), intersect, sort)
import Data.Maybe (maybeToList, isNothing, fromMaybe, listToMaybe)
import Control.Monad (guard, join, liftM2)
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
    testGroup "Starting working makes position selection possible" $
      let prop workplaceSelector = universeProperty $ do
            workplaceId <- pickAnyEmptyWorkplace workplaceSelector
            (playerId, workerId) <- pickWorkerToMove
            applyToUniverse $ startWorking workerId workplaceId
            canSelectPosition <- getsUniverse isSelectingPosition <*> pure playerId
            assert canSelectPosition
      in [
        testProperty "Cut forest" $ prop findEmptyCutForestWorkplaces,
        testProperty "Dig passage" $ prop findEmptyDigPassageWorkplaces,
        testProperty "Gather food" $ prop findEmptyGatherFoodWorkplaces,
        testProperty "Farming" $ prop findEmptyFarmingWorkplaces
      ],
    testGroup "Starting working makes decision available" $
      let prop workplaceSelector precondition decisions = universeProperty $ do
            pre =<< getsUniverse precondition
            workplaceId <- pickAnyEmptyWorkplace workplaceSelector
            (playerId, workerId) <- pickWorkerToMove
            applyToUniverse $ startWorking workerId workplaceId
            availableDecisions <- getsUniverse getPossibleDecisions <*> pure playerId
            stop $ sort availableDecisions === sort decisions
      in [
        testProperty "Digging cave" $ prop findEmptyDigCaveWorkplaces (const True) (CaveOrPassageOption <$> [ChooseCave ..]),
        testProperty "Worker need" $
          prop findEmptyWorkerNeedWorkplaces (liftM2 (||) currentPlayerHasFreeRoom currentPlayerCanBuildRoom) (WorkerNeedOption <$> [HireWorker ..]),
        testProperty "House work" $ prop findEmptyHouseWorkWorkplaces (const True) (AnyRoomOption <$> [ChooseNoRoom ..])
      ],
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
    testGroup "Next player moves worker" $
      let checkNextPlayer previousPlayerId = do
            nextPlayerId <- getsUniverse nextPlayerToMoveWorker <*> pure (previousPlayerId)
            currentPlayerId <- getsUniverse getCurrentPlayer
            assert $ currentPlayerId == nextPlayerId
          playerHasValidOccupants plId = do
            errors <- getsUniverse getOccupantErrors <*> pure plId
            pre $ null errors
      in [
        testProperty "After cutting forest" $ universeProperty $ do
          workplaceId <- pickAnyEmptyWorkplace findEmptyCutForestWorkplaces
          (plId, workerId) <- pickWorkerToMove
          playerHasValidOccupants plId
          applyToUniverse $ startWorking workerId workplaceId
          (position, direction) <- pickSpecificPosition availableForestPositions plId
          applyToUniverse $ selectPosition position direction
          checkNextPlayer plId,
        testProperty "After digging a passage" $ universeProperty $ do
          workplaceId <- pickAnyEmptyWorkplace findEmptyDigPassageWorkplaces
          (plId, workerId) <- pickWorkerToMove
          playerHasValidOccupants plId
          applyToUniverse $ startWorking workerId workplaceId
          (position, direction) <- pickSpecificPosition availableRockPositions plId
          applyToUniverse $ selectPosition position direction
          checkNextPlayer plId
      ],
    -- testGroup "Next player moves worker" $
    --   let checkResultingUniverse nextPlayerId resultUniverse = case nextPlayerId of
    --         Just nextId -> counterexample (show nextId) $ getPlayerStatus resultUniverse nextId === MovingWorker
    --         Nothing -> property $ null [wId | pId <- getPlayers resultUniverse, wId <- getWorkers resultUniverse pId, getWorkerWorkplace resultUniverse wId == Nothing]
    --       coverNextPlayer nextPlayerId = cover (isJust nextPlayerId) 20 "Next player exists" . cover (isNothing nextPlayerId) 20 "No next player exists"
    --       selectPositionProp positionsFunc (ArbitraryUniverse universe) = coverNextPlayer nextPlayerId $ locations /= [] && currentPlayerHasValidOccupants universe
    --         ==> forAll (elements locations) $ \(pos, dir) ->
    --         rightProp $ do
    --           universeAfterSelect <- selectPosition pos dir universe
    --           return $ checkResultingUniverse nextPlayerId universeAfterSelect
    --         where nextPlayerId = nextPlayerToMoveWorker universe Nothing
    --               locations = positionsFunc universe
    --       chooseChildProp (ArbitraryUniverse universe) = coverNextPlayer nextPlayerId $
    --         fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) && currentPlayerHasValidOccupants universe &&
    --         currentPlayerHasFreeRoom universe
    --         ==> rightProp $ do
    --           universeAfterChoose <- chooseOption (WorkerNeedOption HireWorker) universe
    --           return $ counterexample ("Universe after: " ++ ppShow universeAfterChoose) $ checkResultingUniverse nextPlayerId universeAfterChoose
    --         where nextPlayerId = nextPlayerToMoveWorker universe Nothing
    --       chooseProp decisionType selectedOption (ArbitraryUniverse universe) = coverNextPlayer nextPlayerId $
    --         (getPlayerStatus universe <$> getCurrentPlayer universe) == Just (MakingDecision decisionType) && currentPlayerHasValidOccupants universe ==>
    --         rightProp $ do
    --           universeAfterChoose <- chooseOption selectedOption universe
    --           return $ checkResultingUniverse nextPlayerId universeAfterChoose
    --         where nextPlayerId = nextPlayerToMoveWorker universe Nothing
    --       startWorkingProp workplaceFunc (ArbitraryUniverse universe) =
    --         workplaceFunc universe /= [] && findWorkersToMove universe /= [] && currentPlayerHasValidOccupants universe ==>
    --         forAll (elements $ workplaceFunc universe) $ \workplaceId ->
    --         forAll (elements $ findWorkersToMove universe) $ \workerId ->
    --         let nextPlayerId = nextPlayerToMoveWorker universe (Just workerId)
    --         in coverNextPlayer nextPlayerId $
    --           rightProp $ do
    --             nextUniverse <- startWorking workerId workplaceId universe
    --             return $ counterexample (ppShow nextUniverse) $ checkResultingUniverse nextPlayerId nextUniverse
    --       plantCropsProp (ArbitraryUniverse universe) =
    --         (getPlayerStatus universe <$> getCurrentPlayer universe) == (Just PlantingCrops) && currentPlayerHasValidOccupants universe ==>
    --         let nextPlayerId = nextPlayerToMoveWorker universe Nothing
    --         in coverNextPlayer nextPlayerId $
    --           rightProp $ do
    --             nextUniverse <- plantCrops [] universe
    --             return $ counterexample (ppShow nextUniverse) $ checkResultingUniverse nextPlayerId nextUniverse
    --   in [
    --     testProperty "After cutting forest" $ selectPositionProp currentPlayerCutForestLocations,
    --     testProperty "After digging passage" $ selectPositionProp currentPlayerDigPassageLocations,
    --     testProperty "After digging cave" $ selectPositionProp currentPlayerDigCaveLocations,
    --     testProperty "After building a room" $ selectPositionProp currentPlayerBuildLivingRoomLocations,
    --     testProperty "After choosing create child" $ chooseChildProp,
    --     testProperty "After choosing no digging" $ chooseProp CaveOrPassageDecision (CaveOrPassageOption NoDigging),
    --     testProperty "After working in resource addition" $ startWorkingProp findEmptyResourceAdditionWorkplaces,
    --     testProperty "After working in gather wood" $ startWorkingProp findEmptyGatherWoodWorkplaces,
    --     testProperty "After working in make start player" $ startWorkingProp findEmptyMakeStartPlayerWorkplaces,
    --     testProperty "After choosing no room" $ chooseProp AnyRoomDecision (AnyRoomOption ChooseNoRoom),
    --     testProperty "After planting crops" $ plantCropsProp
    --   ],
    -- testGroup "OccupantsInvalid status" $
    --   let prop positionFunc (ArbitraryUniverse universe) = positions /= [] && not (currentPlayerHasValidOccupants universe) ==>
    --         forAll (elements positions) $ \(pos, dir) ->
    --         rightProp $ do
    --           nextUniverse <- selectPosition pos dir universe
    --           let currentPlayerId = getCurrentPlayer universe
    --               nextStatus = getPlayerStatus nextUniverse <$> currentPlayerId
    --           return $ nextStatus == Just OccupantsInvalid
    --         where positions = positionFunc universe
    --       chooseChildProp (ArbitraryUniverse universe) =
    --         fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) && not (currentPlayerHasValidOccupants universe) &&
    --         currentPlayerHasFreeRoom universe
    --         ==> rightProp $ do
    --           universeAfterChoose <- chooseOption (WorkerNeedOption HireWorker) universe
    --           let playerStatus = (getPlayerStatus universeAfterChoose <$> getCurrentPlayer universe)
    --           return $ counterexample (show $ getOccupantErrors universeAfterChoose <$> getCurrentPlayer universe) $ playerStatus === Just OccupantsInvalid
    --       chooseProp decisionType selectedOption (ArbitraryUniverse universe) =
    --         (getPlayerStatus universe <$> getCurrentPlayer universe) == Just (MakingDecision decisionType) && not (currentPlayerHasValidOccupants universe) ==>
    --         rightProp $ do
    --           universeAfterChoose <- chooseOption selectedOption universe
    --           return $ (getPlayerStatus universeAfterChoose <$> getCurrentPlayer universe) === Just OccupantsInvalid
    --       startWorkingProp workplaceFunc (ArbitraryUniverse universe) =
    --         workplaceFunc universe /= [] && findWorkersToMove universe /= [] && not (currentPlayerHasValidOccupants universe) ==>
    --         forAll (elements $ workplaceFunc universe) $ \workplaceId ->
    --         forAll (elements $ findWorkersToMove universe) $ \workerId ->
    --         rightProp $ do
    --           nextUniverse <- startWorking workerId workplaceId universe
    --           return $ (getPlayerStatus nextUniverse <$> getCurrentPlayer universe) == Just OccupantsInvalid
    --       plantCropsProp (ArbitraryUniverse universe) =
    --         (getPlayerStatus universe <$> getCurrentPlayer universe) == (Just PlantingCrops) && not (currentPlayerHasValidOccupants universe) ==>
    --         rightProp $ do
    --             nextUniverse <- plantCrops [] universe
    --             return $ counterexample (ppShow nextUniverse) $ (getPlayerStatus nextUniverse <$> getCurrentPlayer universe) == Just OccupantsInvalid
    --   in [
    --     testProperty "Cutting a forest" $ prop currentPlayerCutForestLocations,
    --     testProperty "Digging a passage" $ prop currentPlayerDigPassageLocations,
    --     testProperty "Digging a cave" $ prop currentPlayerDigCaveLocations,
    --     testProperty "Building a room" $ prop currentPlayerBuildLivingRoomLocations,
    --     testProperty "Choosing create child" $ chooseChildProp,
    --     testProperty "Choosing no digging" $ chooseProp CaveOrPassageDecision (CaveOrPassageOption NoDigging),
    --     testProperty "Starting working in resource addition" $ startWorkingProp findEmptyResourceAdditionWorkplaces,
    --     testProperty "Starting working in gather wood" $ startWorkingProp findEmptyGatherWoodWorkplaces,
    --     testProperty "After working in make start player" $ startWorkingProp findEmptyMakeStartPlayerWorkplaces,
    --     testProperty "Choosing no room" $ chooseProp AnyRoomDecision (AnyRoomOption ChooseNoRoom),
    --     testProperty "Planting crops" $ plantCropsProp
    --   ],
    -- testProperty "Fixing occupants in invalid occupants starts next player" $
    --   let prop (ArbitraryUniverse universe) = currentPlayerIsInInvalidOccupantsState ==> either (error) id $ do
    --         let currentPlayerId = fromJust $ getCurrentPlayer universe
    --             occupants = createValidOccupants universe currentPlayerId
    --         nextUniverse <- alterOccupants currentPlayerId occupants universe
    --         let nextPlayerMovingWorker = (getPlayerStatus nextUniverse <$> getCurrentPlayer nextUniverse) == Just MovingWorker
    --             currentPlayerWaiting = (getPlayerStatus nextUniverse currentPlayerId) `elem` [Waiting, MovingWorker]
    --         return $ (currentPlayerWaiting && nextPlayerMovingWorker) || allPlayersWaiting nextUniverse
    --         where currentPlayerIsInInvalidOccupantsState = (getPlayerStatus universe <$> getCurrentPlayer universe) == Just OccupantsInvalid
    --   in prop,
    -- testProperty "Player not moving worker cannot move worker" $
    --   let prop (ArbitraryUniverse universe) =
    --         forAll (elements $ getPlayers universe) $ \playerId ->
    --         hasOtherStatus playerId && getMovableWorkers playerId /= [] && findEmptyWorkplaces universe /= [] ==>
    --           forAll (elements $ getWorkers universe playerId) $ \workerId ->
    --           forAll (elements $ keys $ getWorkplaces universe) $ \workplaceId ->
    --           leftProp $ startWorking workerId workplaceId universe
    --         where hasOtherStatus plId = getPlayerStatus universe plId /= MovingWorker
    --               getMovableWorkers plId = [wId | wId <- getWorkers universe plId, getWorkerWorkplace universe wId == Nothing]
    --   in prop,
    -- testGroup "Selecting position alters building space" $
    --   let prop positionFunc buildingConstructors (ArbitraryUniverse universe) = positionFunc universe /= [] ==>
    --         forAll (elements $ positionFunc universe) $ \(pos, dir) ->
    --         rightProp $ do
    --           nextUniverse <- selectPosition pos dir universe
    --           let buildings = zipWith id buildingConstructors [pos, pos ^+^ directionAddition dir]
    --               buildingSpace = getBuildingSpace nextUniverse currentPlayerId
    --           return $ counterexample ("Expected: " ++ show buildings ++ "\n Building Space: "++ show buildingSpace) $ intersect buildings buildingSpace == buildings
    --         where currentPlayerId = fromJust $ getCurrentPlayer universe
    --   in [
    --     testProperty "Cutting forest" $ prop currentPlayerCutForestLocations [Grass, Field],
    --     testProperty "Digging passage" $ prop currentPlayerDigPassageLocations [Cave, Passage],
    --     testProperty "Digging cave" $ prop currentPlayerDigCaveLocations [Cave, Cave],
    --     testProperty "Building living room" $ prop currentPlayerBuildLivingRoomLocations [LivingRoom]
    --   ],
    -- testGroup "Selecting a position when not supposed to fails" $
    --   let prop playerStatuses positionsFunc (ArbitraryUniverse universe) =
    --         forAll (elements $ playersToChooseFrom) $ \playerId ->
    --         (getPlayerStatus universe <$> getCurrentPlayer universe) `notElem` (Just <$> playerStatuses)
    --           && positionsFunc universe playerId /= [] ==>
    --         forAll (elements $ positionsFunc universe playerId) $ \(pos, dir) ->
    --         leftProp $ selectPosition pos dir universe
    --         where playersToChooseFrom = fromMaybe (getPlayers universe) $ return <$> getCurrentPlayer universe
    --   in [
    --     testProperty "Cutting Forest" $ prop [CuttingForest] availableForestPositions,
    --     testProperty "Digging" $ prop [DiggingPassage, DiggingCave] availableRockPositions,
    --     testProperty "Building room" $ prop [BuildingLivingRoom] availableSingleCavePositions
    --   ],
    -- testGroup "Selecting a wrong position fails" $
    --   let prop playerStatus positionsFunc (ArbitraryUniverse universe) = currentPlayerCuttingForest ==>
    --         forAll (elements wrongPositions) $ \(pos, dir) ->
    --         leftProp $ selectPosition pos dir universe
    --         where currentPlayerCuttingForest = (getPlayerStatus universe <$> getCurrentPlayer universe) == Just playerStatus
    --               wrongPositions = S.toList $ S.fromList [((x, y), dir) | x <- [-1..6], y <- [-1..4], dir <- allDirections]
    --                 S.\\ S.fromList (positionsFunc universe (fromJust $ getCurrentPlayer universe))
    --   in [
    --     testProperty "Cutting Forest" $ prop CuttingForest availableForestPositions,
    --     testProperty "Digging Passage" $ prop DiggingPassage availableRockPositions,
    --     testProperty "Digging Cave" $ prop DiggingCave availableRockPositions,
    --     testProperty "Building a Living Room" $ prop BuildingLivingRoom availableSingleCavePositions
    --   ],
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
                areWorkplaceDataOk ResourceAddition ResourceAddition = True
                areWorkplaceDataOk (GatherWood orig) (GatherWood new) = new == orig + 1
                areWorkplaceDataOk (GatherFood orig) (GatherFood new) = new == orig + 1
                areWorkplaceDataOk (MakeStartPlayer orig) (MakeStartPlayer new) = new == orig + 1
                areWorkplaceDataOk HouseWork HouseWork = True
                areWorkplaceDataOk Farming Farming = True
                areWorkplaceDataOk _ _ = False
            return $ all isWorkplaceId (keys originalWorkplaces)
      in prop,
    -- testProperty "Canceling selection starts next worker" $
    --   let prop (ArbitraryUniverse universe) = currentPlayerSelectingPosition && nextPlayerHasWorker ==> rightProp $ do
    --         nextUniverse <- cancelSelection universe
    --         return $ if currentPlayerId == nextPlayerId
    --           then (getPlayerStatus nextUniverse <$> currentPlayerId) == Just MovingWorker
    --           else (getPlayerStatus nextUniverse <$> currentPlayerId) == Just Waiting && (getPlayerStatus nextUniverse <$> nextPlayerId) == Just MovingWorker
    --         where currentPlayerSelectingPosition = (getPlayerStatus universe <$> getCurrentPlayer universe) `elem` (Just <$> cancellableSelectingPositionStatuses)
    --               currentPlayerId = getCurrentPlayer universe
    --               nextPlayerId = head . tail $ (dropWhile (/=currentPlayerId) (Just <$> cycle (getPlayers universe)))
    --               nextPlayerHasWorker = any (isNothing . getWorkerWorkplace universe) $ (join . maybeToList) $ getWorkers universe <$> nextPlayerId
    --   in prop,
    -- testProperty "Canceling selection doesn't change workers" $
    --   let prop (ArbitraryUniverse universe) = currentPlayerSelectingPosition ==> rightProp $ do
    --         nextUniverse <- cancelSelection universe
    --         let workerPositions u = [getWorkerWorkplace u wId | pId <- getPlayers u, wId <- getWorkers u pId]
    --         return $ workerPositions universe == workerPositions nextUniverse
    --         where currentPlayerSelectingPosition = (getPlayerStatus universe <$> getCurrentPlayer universe) `elem` (Just <$> cancellableSelectingPositionStatuses)
    --   in prop,
    testGroup "Adding resources" $
      let getWorkplaceWoodAmount (CutForest n) = n
          getWorkplaceWoodAmount (GatherWood n) = n
          getWorkplaceWoodAmount _ = 0
          getWorkplaceStoneAmount (DigCave n) = n
          getWorkplaceStoneAmount (DigPassage n) = n
          getWorkplaceStoneAmount _ = 0
          getWorkplaceFoodAmount (GatherFood n) = n
          getWorkplaceFoodAmount (MakeStartPlayer n) = n
          getWorkplaceFoodAmount _ = 0
          prop workplacesFunc resourceFunctions (ArbitraryUniverse universe) = findWorkersToMove universe /= [] && workplacesFunc universe /= [] ==>
            forAll (elements $ findWorkersToMove universe) $ \(_, workerId) ->
            forAll (elements $ workplacesFunc universe) $ \workplaceId ->
            rightProp $ do
            nextUniverse <- startWorking workerId workplaceId universe
            let playerId = head $ [pId | pId <- getPlayers universe, wId <- getWorkers universe pId, wId == workerId]
                checkResource (resourceFunc, workplaceResourceFunc) = newAmount - origAmount == origWorkplaceAmount
                  where origAmount = resourceFunc $ getPlayerResources universe playerId
                        newAmount = resourceFunc $ getPlayerResources nextUniverse playerId
                        origWorkplaceAmount = workplaceResourceFunc $ (getWorkplaces universe) ! workplaceId
            return $ all checkResource resourceFunctions
      in [
        testProperty "Cutting forest" $ prop findEmptyCutForestWorkplaces [(getWoodAmount, getWorkplaceWoodAmount)],
        testProperty "Digging passage" $ prop findEmptyDigPassageWorkplaces [(getStoneAmount, getWorkplaceStoneAmount)],
        testProperty "Digging cave" $ prop findEmptyDigCaveWorkplaces [(getStoneAmount, getWorkplaceStoneAmount)],
        testProperty "Resource addition" $ prop findEmptyResourceAdditionWorkplaces
          [(getStoneAmount, const 1), (getWoodAmount, const 1), (getIronAmount, const 1), (getFoodAmount, const 1), (getGoldAmount, const 1)],
        testProperty "Gather wood" $ prop findEmptyGatherWoodWorkplaces [(getWoodAmount, getWorkplaceWoodAmount)],
        testProperty "Gather food" $ prop findEmptyGatherFoodWorkplaces [(getFoodAmount, getWorkplaceFoodAmount), (getWheatAmount, const 1)],
        testProperty "Make start worker" $ prop findEmptyMakeStartPlayerWorkplaces [(getFoodAmount, getWorkplaceFoodAmount), (getIronAmount, const 2)]
      ],
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
      in prop,
    -- testProperty "Canceling selection is not possible when building living room" $
    --   let prop (ArbitraryUniverse universe) = (getPlayerStatus universe <$> getCurrentPlayer universe) == Just BuildingLivingRoom ==>
    --         leftProp $ do
    --           cancelSelection universe
    --   in prop,
    -- testProperty "Choosing build room changes state to BuildingLivingRoom" $
    --   let prop (ArbitraryUniverse universe) =
    --         fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) &&
    --           availableSingleCavePositions universe (fromJust $ getCurrentPlayer universe) /= [] && currentPlayerHasEnoughResourcesForLivingRoom universe ==>
    --         rightProp $ do
    --           nextUniverse <- chooseOption (WorkerNeedOption BuildRoom) universe
    --           return $ getPlayerStatus nextUniverse (fromJust $ getCurrentPlayer universe) == BuildingLivingRoom
    --   in prop,
    -- testProperty "Choosing build room subtracts resources" $
    --   let prop (ArbitraryUniverse universe) =
    --         fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) &&
    --           availableSingleCavePositions universe (fromJust $ getCurrentPlayer universe) /= [] && currentPlayerHasEnoughResourcesForLivingRoom universe ==>
    --         rightProp $ do
    --           nextUniverse <- chooseOption (WorkerNeedOption BuildRoom) universe
    --           let currentPlayerId = fromJust $ getCurrentPlayer universe
    --               origResources = getPlayerResources universe currentPlayerId
    --               newResources = getPlayerResources nextUniverse currentPlayerId
    --           return $ getWoodAmount newResources == getWoodAmount origResources - 4 && getStoneAmount newResources == getStoneAmount origResources - 3
    --   in prop,
    -- testProperty "Choosing build room fails when there are no caves available" $
    --   let prop (ArbitraryUniverse universe) =
    --         fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) &&
    --           availableSingleCavePositions universe (fromJust $ getCurrentPlayer universe) == [] && currentPlayerHasEnoughResourcesForLivingRoom universe==>
    --         leftProp $ chooseOption (WorkerNeedOption BuildRoom) universe
    --   in prop,
    -- testProperty "Choosing build room fails when there aren't enough resources" $
    --   let prop (ArbitraryUniverse universe) =
    --         fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) &&
    --           availableSingleCavePositions universe (fromJust $ getCurrentPlayer universe) /= [] && not (currentPlayerHasEnoughResourcesForLivingRoom universe) ==>
    --         leftProp $ chooseOption (WorkerNeedOption BuildRoom) universe
    --   in prop,
    -- testProperty "Choosing make child fails when there is no room available" $
    --   let prop (ArbitraryUniverse universe) =
    --         (not $ currentPlayerHasFreeRoom universe) && fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) ==>
    --         leftProp $ chooseOption (WorkerNeedOption HireWorker) universe
    --   in prop,
    -- testProperty "Choosing make child creates a new worker" $
    --   let prop (ArbitraryUniverse universe) =
    --         currentPlayerHasFreeRoom universe && fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) ==>
    --         rightProp $ do
    --           nextUniverse <- chooseOption (WorkerNeedOption HireWorker) universe
    --           let currentPlayerId = fromJust $ getCurrentPlayer universe
    --               currentPlayerOrigWorkers = getWorkers universe currentPlayerId
    --               currentPlayerNewWorkers = getWorkers nextUniverse currentPlayerId
    --               allWorkerIds = [wId | plId <- getPlayers nextUniverse, wId <- getWorkers nextUniverse plId]
    --           return $ length currentPlayerNewWorkers == length currentPlayerOrigWorkers + 1 && length allWorkerIds == S.size (S.fromList allWorkerIds)
    --   in prop,
    -- testProperty "Choosing make child keeps occupants valid" $
    --   let prop (ArbitraryUniverse universe) =
    --         currentPlayerHasFreeRoom universe && fromMaybe False (isChoosingWorkerNeed <$> getPlayerStatus universe <$> getCurrentPlayer universe) &&
    --         (getOccupantErrors universe <$> getCurrentPlayer universe) == Just [] ==>
    --         rightProp $ do
    --           nextUniverse <- chooseOption (WorkerNeedOption HireWorker) universe
    --           let currentPlayerId = fromJust $ getCurrentPlayer universe
    --           return $ getOccupantErrors nextUniverse currentPlayerId == []
    --   in prop,
    -- testProperty "Choosing cave changes state to DiggingCave" $
    --   let prop (ArbitraryUniverse universe) =
    --         (getPlayerStatus universe <$> getCurrentPlayer universe) == Just (MakingDecision CaveOrPassageDecision) ==>
    --         rightProp $ do
    --           nextUniverse <- chooseOption (CaveOrPassageOption ChooseCave) universe
    --           return $ getPlayerStatus nextUniverse (fromJust $ getCurrentPlayer universe) == DiggingCave
    --   in prop,
    -- testProperty "Choosing passage changes state to DiggingPassage" $
    --   let prop (ArbitraryUniverse universe) =
    --         (getPlayerStatus universe <$> getCurrentPlayer universe) == Just (MakingDecision CaveOrPassageDecision) ==>
    --         rightProp $ do
    --           nextUniverse <- chooseOption (CaveOrPassageOption ChoosePassage) universe
    --           return $ getPlayerStatus nextUniverse (fromJust $ getCurrentPlayer universe) == DiggingPassage
    --   in prop,
    testGroup "Starting working clears workplace" $
      let prop workplaceFunc emptyWorkplace (ArbitraryUniverse universe) = workplaceFunc universe /= [] && findWorkersToMove universe /= [] ==>
            forAll (elements $ workplaceFunc universe) $ \workplaceId ->
            forAll (elements $ findWorkersToMove universe) $ \(_, workerId) ->
            rightProp $ do
              nextUniverse <- startWorking workerId workplaceId universe
              let workplaceData = getWorkplaces nextUniverse ! workplaceId
              return $ workplaceData == emptyWorkplace
      in [
        testProperty "cut forest" $ prop findEmptyCutForestWorkplaces (CutForest 0),
        testProperty "dig cave" $ prop findEmptyDigCaveWorkplaces (DigCave 0),
        testProperty "dig passage" $ prop findEmptyDigPassageWorkplaces (DigPassage 0),
        testProperty "gather wood" $ prop findEmptyGatherWoodWorkplaces (GatherWood 0),
        testProperty "gather food" $ prop findEmptyGatherFoodWorkplaces (GatherFood 0),
        testProperty "make start player" $ prop findEmptyMakeStartPlayerWorkplaces (MakeStartPlayer 0)
      ],
    testProperty "After working in make start player, starting player is changed to current player" $
      let prop (ArbitraryUniverse universe) = findEmptyMakeStartPlayerWorkplaces universe /= [] && findWorkersToMove universe /= [] ==>
            forAll (elements $ findEmptyMakeStartPlayerWorkplaces universe) $ \workplaceId ->
            forAll (elements $ findWorkersToMove universe) $ \(_, workerId) ->
            rightProp $ do
              nextUniverse <- startWorking workerId workplaceId universe
              return (Just (getStartingPlayer nextUniverse) == getCurrentPlayer universe)
      in prop --,
    -- testProperty "When working in house work, dog is added" $
    --   let prop (ArbitraryUniverse universe) = findEmptyHouseWorkWorkplaces universe /=[] && findWorkersToMove universe /= [] ==>
    --         forAll (elements $ findEmptyHouseWorkWorkplaces universe) $ \workplaceId ->
    --         forAll (elements $ findWorkersToMove universe) $ \workerId ->
    --         rightProp $ do
    --           nextUniverse <- startWorking workerId workplaceId universe
    --           let currentPlayerId = fromJust $ getCurrentPlayer universe
    --           return $ length (getDogs universe currentPlayerId) + 1 == length (getDogs nextUniverse currentPlayerId)
    --   in prop,
    -- testProperty "After working in house work, dogs ids are distinct" $
    --   let prop (ArbitraryUniverse universe) = findEmptyHouseWorkWorkplaces universe /= [] && findWorkersToMove universe /= [] ==>
    --         forAll (elements $ findEmptyHouseWorkWorkplaces universe) $ \workplaceId ->
    --         forAll (elements $ findWorkersToMove universe) $ \workerId ->
    --         rightProp $ do
    --           nextUniverse <- startWorking workerId workplaceId universe
    --           let nextDogs = [dogId | plId <- getPlayers nextUniverse, dogId <- getDogs nextUniverse plId]
    --           return $ counterexample ("New dogs: " ++ show nextDogs) $ nextDogs == nub nextDogs
    --   in prop
  ]

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

pickSpecificPosition :: (Universe -> PlayerId -> [(Position, Direction)]) -> PlayerId -> UniversePropertyMonad (Position, Direction)
pickSpecificPosition func plId = do
  positions <- getsUniverse func <*> pure plId
  pre $ not $ null $ positions
  pick $ elements positions

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

nextPlayerToMoveWorker :: Universe -> PlayerId -> Maybe PlayerId
nextPlayerToMoveWorker universe currentPlayerId =
  let furtherPlayerIds = tail $ dropWhile (/= currentPlayerId) $ getPlayers universe ++ getPlayers universe
      isWorkerFree workerId = isNothing (getWorkerWorkplace universe workerId)
      playersWithFreeWorkers = [plId | plId <- furtherPlayerIds, any isWorkerFree (getWorkers universe plId)]
  in listToMaybe playersWithFreeWorkers

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

-- isChoosingWorkerNeed :: PlayerStatus -> Bool
-- isChoosingWorkerNeed (MakingDecision (WorkerNeedDecision _)) = True
-- isChoosingWorkerNeed _ = False

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
