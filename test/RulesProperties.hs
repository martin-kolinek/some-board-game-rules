module RulesProperties where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Data.Map (keys, (!), elems, findWithDefault, alter)
import qualified Data.Map as M
import Data.List ((\\), intersect)
import Data.Maybe (maybeToList, isNothing, fromMaybe)
import Control.Monad (guard, join, forM_, liftM)
import Text.Show.Pretty (ppShow)

import Generators
import Rules
import TestFramework
import TestHelpers

rulesPropertiesTests :: TestTree
rulesPropertiesTests = localOption (QuickCheckMaxRatio 500) $ testGroup "Rules properties" [
    testProperty "Starting working assigns worker" $ generalUniverseProperty $ do
        workplaceId <- pickEmptyWorkplace
        (plId, workerId) <- pickWorkerToMove
        checkPlayerHasValidOccupants plId
        do
          workplaces <- getsUniverse getWorkplaces
          canBuildRoom <- getsUniverse currentPlayerCanBuildRoom
          hasFreeRoom <- getsUniverse currentPlayerHasFreeRoom
          canArmWorker <- getsUniverse (currentPlayerCanArmWorker workerId)
          pre $ getWorkplaceType (workplaces ! workplaceId) /= WorkerNeed || canBuildRoom || hasFreeRoom
          pre $ getWorkplaceType (workplaces ! workplaceId) /= WeaponMaking || canArmWorker
        applyToUniverse $ startWorking plId workerId workplaceId
        workerWorkplace <- getsUniverse (flip getWorkerWorkplace workerId)
        assert $ workerWorkplace == Just workplaceId
    ,
    testProperty "Finishing turn unassigns all workers" $ generalUniverseProperty $ do
      pre =<< getsUniverse allPlayersWaiting
      applyToUniverse finishTurn
      updatedUniverse <- getUniverse
      let workerFree = (== Nothing) . getWorkerWorkplace updatedUniverse
          workers = [wId | plId <- getPlayers updatedUniverse, wId <- getWorkers updatedUniverse plId]
      assert $ all workerFree workers,
    testProperty "Finishing turn starts starting player" $ generalUniverseProperty $ do
      pre =<< getsUniverse allPlayersWaiting
      startingPlayer <- getsUniverse getStartingPlayer
      applyToUniverse finishTurn
      currentPlayer <- getsUniverse getCurrentPlayer
      assert $ currentPlayer == Just startingPlayer,
    testProperty "Finishing turn is not possible without all players waiting" $ generalUniverseProperty $ do
      pre =<< getsUniverse (not . allPlayersWaiting)
      currentPlayer <- getsUniverse getCurrentPlayer
      monitor (counterexample $ "Current player: " ++ show currentPlayer)
      applyToUniverse finishTurn
      shouldHaveFailed,
    testProperty "Moving same player twice causes an error" $ generalUniverseProperty $ do
      universe <- getUniverse
      let workersToMove = findWorkersToMove universe
          workplaces = findEmptyWorkplaces universe
          playersAbleToMove = [plId | plId <- getPlayers universe, any (isNothing . getWorkerWorkplace universe) (getWorkers universe plId)]
      pre $ length playersAbleToMove >= 2
      pre $ length workersToMove >= 2
      pre $ length workplaces >= 2
      applyToUniverse $ startWorking (fst $ workersToMove !! 0) (snd $ workersToMove !! 0) (workplaces !! 0)
      applyToUniverse $ startWorking (fst $ workersToMove !! 1) (snd $ workersToMove !! 1) (workplaces !! 1)
      shouldHaveFailed,
    testProperty "Getting workplace workers works" $ \(ArbitraryUniverse universe) ->
        (not . null) (findOccupiedWorkplaces universe) ==>
        forAll (elements $ findOccupiedWorkplaces universe) $ \workplaceId ->
        let occupants = getWorkplaceOccupants universe workplaceId
            workplaceIsNotEmpty = (not . null) occupants
            workerWorksInWorkplace workerId = getWorkerWorkplace universe workerId == Just workplaceId
            allWorkersWorkInWorkplace = all workerWorksInWorkplace occupants
        in workplaceIsNotEmpty && allWorkersWorkInWorkplace,
    testProperty "Player not moving worker cannot move worker" $ generalUniverseProperty $ do
      players <- getsUniverse getPlayers
      playerId <- pick $ elements players
      universe <- getUniverse
      pre $ not $ isMovingWorker universe playerId
      forM_ (getWorkers universe playerId) $ \workerId ->
        forM_ (findEmptyWorkplaces universe) $ \workplaceId ->
                case startWorking playerId workerId workplaceId universe of
                  Left _ -> return ()
                  Right u -> do
                               monitor $ counterexample ("Could move worker " ++ show workerId ++ " to " ++ show workplaceId ++ ", result: \n" ++ ppShow u)
                               assert False
      return (),
    testProperty "Reverting occupants returns original errors" $
      let prop (ArbitraryUniverse universe) =
            forAll (elements $ getPlayers universe) $ \playerId ->
            forAll (shuffleOccupantsForPlayer universe playerId) $ \newOccupants ->
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
          correct universe playerId = (AnimalOccupant <$> getAnimals universe playerId) ++ (WorkerOccupant <$> getWorkers universe playerId) == getAllOccupants universe playerId
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
              return $ not $ null $ errors
            where playersWithNoOccupantErrors = [plId | plId <- getPlayers universe, getOccupantErrors universe plId == []]
                  originalOccupants playerId = getBuildingOccupants universe playerId
                  occupantsToMove playerId = filter isWorker $ join $ elems $ originalOccupants playerId
                  isWorker (WorkerOccupant _) = True
                  isWorker _ = False
                  isPositionInvalid playerId pos = intersect [SmallBuilding InitialRoom pos, SmallBuilding LivingRoom pos] (getBuildingSpace universe playerId) == []
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
      let prop (ArbitraryUniverse universe) =
            forAll (elements $ getPlayers universe) $ \playerId ->
            forAll (elements $ join $ elems $ originalOccupants playerId) $ \occupant ->
            playerCanPlaceOccupant universe playerId occupant ==>
            rightProp $ do
              let newOccupants = positionOccupants (getBuildingSpace universe playerId) (originalOccupants playerId) occupant
              nextUniverse <- alterOccupants playerId newOccupants universe
              let errors = getOccupantErrors nextUniverse playerId
              return $
                counterexample ("new occupants: " ++ show newOccupants) $
                counterexample ("original occupants: " ++ show (originalOccupants playerId)) $
                not $ null errors
            where originalOccupants playerId = getBuildingOccupants universe playerId
      in prop,
    testProperty "Having occupant from different player causes error" $
      let prop (ArbitraryUniverse universe) =
            forAll (elements $ getPlayers universe) $ \otherPlayerId ->
            forAll (elements $ join $ elems $ originalOccupants otherPlayerId) $ \occupant ->
            let playersWithFreeBuilding = [plId | plId <- getPlayers universe, playerCanPlaceOccupant universe plId occupant]
            in playersWithFreeBuilding /= [] && length (getPlayers universe) > 1 ==>
            forAll (elements $ playersWithFreeBuilding) $ \playerId ->
            rightProp $ do
              let newOccupants = positionOccupants (getBuildingSpace universe playerId) (originalOccupants playerId) occupant
              nextUniverse <- alterOccupants playerId newOccupants universe
              let errors = getOccupantErrors nextUniverse playerId
              return $ counterexample (ppShow nextUniverse) $ not $ null errors
            where originalOccupants playerId = getBuildingOccupants universe playerId
      in prop,
    testProperty "Finishing turn removes crops" $ generalUniverseProperty $ do
      pre =<< getsUniverse allPlayersWaiting
      universe <- getUniverse
      let cropsExist plId = not $ M.null $ getPlantedCrops universe plId
      pre $ any cropsExist (getPlayers universe)
      applyToUniverse finishTurn
      forM_ (getPlayers universe) $ \playerId -> do
        let validatePlantedCrop (Just (PlantedCrop origType origCount)) (Just (PlantedCrop newType newCount)) = origType == newType && newCount == origCount - 1
            validatePlantedCrop (Just (PlantedCrop _ 1)) Nothing = True
            validatePlantedCrop Nothing Nothing = True
            validatePlantedCrop _ _ = False
            originalPlantedCrops = getPlantedCrops universe playerId
        newPlantedCrops <- getsUniverse getPlantedCrops <*> pure playerId
        forM_ availableBuildingPositions $ \position -> do
          if (validatePlantedCrop (M.lookup position originalPlantedCrops) (M.lookup position newPlantedCrops))
            then return ()
            else do
              monitor $ counterexample $ "Player = " ++ (show playerId) ++ " position = " ++ (show position)
              assert False,
    testProperty "Finishing turn adds crop resources" $ generalUniverseProperty $ do
      pre =<< getsUniverse allPlayersWaiting
      universe <- getUniverse
      applyToUniverse finishTurn
      let cropsExist plId = not $ M.null $ getPlantedCrops universe plId
      pre $ any cropsExist (getPlayers universe)
      forM_ (getPlayers universe) $ \playerId -> do
        let countCropType tp (PlantedCrop plantedType _) = if tp == plantedType then 1 else 0
            countCrop tp = sum $ countCropType tp <$> (M.elems $ getPlantedCrops universe playerId)
            cropAmount Potatoes = getPotatoAmount
            cropAmount Wheat = getWheatAmount
            originalResources = getPlayerResources universe playerId
        newResources <- getsUniverse getPlayerResources <*> pure playerId
        let verifyCrop tp = cropAmount tp newResources - cropAmount tp originalResources == countCrop tp
        assert $ verifyCrop Potatoes
        assert $ verifyCrop Wheat
      return (),
    testProperty "There are no planted crops with zero" $ generalUniverseProperty $ do
      pre =<< getsUniverse allPlayersWaiting
      universe <- getUniverse
      applyToUniverse finishTurn
      let cropsExist plId = not $ M.null $ getPlantedCrops universe plId
          plantedCropIsValid (PlantedCrop _ 0) = False
          plantedCropIsValid _ = True
      pre $ any cropsExist (getPlayers universe)
      forM_ (getPlayers universe) $ \playerId -> do
        crops <- getsUniverse getPlantedCrops <*> pure playerId
        assert $ all plantedCropIsValid $ elems crops,
    testProperty "When can collect resources then can collect resources" $ generalUniverseProperty $ do
      playerId <- findFittingPlayer canCollectResources
      applyToUniverse $ collectResources playerId,
    testProperty "When can hire worker then can hire worker" $ generalUniverseProperty $ do
      playerId <- findFittingPlayer canHireWorker
      applyToUniverse $ hireWorker playerId,
    testProperty "When can finish action then can finish action" $ generalUniverseProperty $ do
      playerId <- findFittingPlayer canFinishAction
      applyToUniverse $ finishAction playerId,
    testProperty "When cannot collect resources then cannot collect resources" $ generalUniverseProperty $ do
      playerId <- findFittingPlayer $ liftM not . canCollectResources
      applyToUniverse $ collectResources playerId
      shouldHaveFailed,
    testProperty "When can hire worker then cannot hire worker" $ generalUniverseProperty $ do
      playerId <- findFittingPlayer $ liftM not . canHireWorker
      applyToUniverse $ hireWorker playerId
      shouldHaveFailed,
    testProperty "When can finish action then cannot finish action" $ generalUniverseProperty $ do
      playerId <- findFittingPlayer $ liftM not . canFinishAction
      applyToUniverse $ finishAction playerId
      shouldHaveFailed,
    testProperty "Moving worker with invalid occupants is not possible" $ generalUniverseProperty $ do
      workplaceId <- pickEmptyWorkplace
      (plId, workerId) <- pickWorkerToMove
      checkPlayerHasInvalidOccupants plId
      applyToUniverse $ startWorking plId workerId workplaceId
      shouldHaveFailed
  ]

allPossibleOptions :: [Options]
allPossibleOptions = (WorkerNeedOption <$> [HireWorker ..]) ++ (CaveOrPassageOption <$> [ChooseCave ..]) ++ (ArmOption <$> (ArmWorker <$> [-2..10]) ++ [NoArming])

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

rightProp :: Testable a => Either String a -> Property
rightProp result = case result of
  Left msg -> counterexample ("Should be successful but is: " ++ msg) False
  Right prop -> property prop

leftProp :: Show a => Either String a -> Property
leftProp result = case result of
  Left _ -> property True
  Right x -> counterexample ("Expected error but got: " ++ ppShow x) False

findOccupiedWorkplaces :: Universe -> [WorkplaceId]
findOccupiedWorkplaces universe = do
  playerId <- getPlayers universe
  workerId <- getWorkers universe playerId
  maybeToList $ getWorkerWorkplace universe workerId

findEmptyWorkplaces :: Universe -> [WorkplaceId]
findEmptyWorkplaces universe = (keys $ getWorkplaces universe) \\ findOccupiedWorkplaces universe

findWorkersToMove :: Universe -> [(PlayerId, WorkerId)]
findWorkersToMove universe = do
  playerId <- getPlayers universe
  guard $ isMovingWorker universe playerId
  workerId <- getWorkers universe playerId
  guard $ getWorkerWorkplace universe workerId == Nothing
  return (playerId, workerId)

allPlayersWaiting :: Universe -> Bool
allPlayersWaiting universe = getCurrentPlayer universe == Nothing

positionOccupants :: [Building] -> BuildingOccupants -> BuildingOccupant -> BuildingOccupants
positionOccupants buildings oldOccupants newOccupant =
  let position = case head $ filter (canBuildingSupportAdditionalOccupant oldOccupants newOccupant) buildings of
        (SmallBuilding _ pos) -> pos
        (LargeBuilding _ pos _) -> pos
  in alter (\old -> Just $ newOccupant : fromMaybe [] old) position oldOccupants

findFittingPlayer :: (Universe -> PlayerId -> Bool) -> UniversePropertyMonad PlayerId
findFittingPlayer condition = do
  players <- getsUniverse getPlayers
  u <- getUniverse
  let fittingPlayers = filter (condition u) players
  pre $ not $ null $ fittingPlayers
  pick $ elements fittingPlayers

isWorkerOccupant :: BuildingOccupant -> Bool
isWorkerOccupant (WorkerOccupant _) = True
isWorkerOccupant _ = False

isAnimalOccupant :: AnimalType -> BuildingOccupant -> Bool
isAnimalOccupant animalType (AnimalOccupant (Animal animalType2 _)) = animalType == animalType2
isAnimalOccupant _ _ = False

isOtherFarmAnimalOccupant :: FarmAnimalType -> BuildingOccupant -> Bool
isOtherFarmAnimalOccupant animalType (AnimalOccupant (Animal (FarmAnimalType animalType2) _)) = animalType /= animalType2
isOtherFarmAnimalOccupant _ _ = False

playerCanPlaceOccupant :: Universe -> PlayerId -> BuildingOccupant -> Bool
playerCanPlaceOccupant universe plId occupant =
  let space = getBuildingSpace universe plId
      occupants = getBuildingOccupants universe plId
  in any (canBuildingSupportAdditionalOccupant occupants occupant) space

canBuildingSupportAdditionalOccupant :: BuildingOccupants -> BuildingOccupant -> Building -> Bool
canBuildingSupportAdditionalOccupant _ (AnimalOccupant (Animal Dog _)) _ = True
canBuildingSupportAdditionalOccupant occupants (AnimalOccupant (Animal (FarmAnimalType animalType) _)) building =
  otherFarmAnimalCount == 0 && canBuildingSupportAdditionalFarmAnimal animalType
  where currentSameTypeAnimalCount = length $ filter (isAnimalOccupant (FarmAnimalType animalType)) existingOccupants
        positions = buildingPositions building
        existingOccupants = [x | pos <- positions, x <- findWithDefault [] pos occupants]
        otherFarmAnimalCount = length $ filter (isOtherFarmAnimalOccupant animalType) existingOccupants
        canBuildingSupportAdditionalFarmAnimal Sheep =
          let dogAmount = length $ filter (isAnimalOccupant Dog) existingOccupants
              supportedSheep = case building of
                (SmallBuilding Grass _) -> dogAmount + 1
                (SmallBuilding SmallPasture _) -> max (dogAmount + 1) 2
                (LargeBuilding LargePasture _ _) -> max (dogAmount + 1) 4
                _ -> 0
          in currentSameTypeAnimalCount < supportedSheep
        canBuildingSupportAdditionalFarmAnimal Cow =
          let supportedCows = case building of
                (SmallBuilding SmallPasture _) -> 2
                (LargeBuilding LargePasture _ _) -> 4
                _ -> 0
          in currentSameTypeAnimalCount < supportedCows
canBuildingSupportAdditionalOccupant occupants (WorkerOccupant _) (SmallBuilding buildingType position) =
  let supportedWorkers = case buildingType of
        LivingRoom -> 1
        InitialRoom -> 2
        _ -> 0
      existingWorkers = length $ filter isWorkerOccupant $ findWithDefault [] position occupants
  in existingWorkers < supportedWorkers
canBuildingSupportAdditionalOccupant _ (WorkerOccupant _) _ = False
