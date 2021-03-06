{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Universe.Interaction where

import Prelude hiding (lookup)
import Control.Monad.Except
import Control.Lens hiding (universe)
import Data.List (sortOn, groupBy, foldl')
import Data.Function (on)
import Data.AdditiveGroup
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Universe.Workplace
import Universe.Actions
import Universe.Building
import Universe.Player
import Universe.Worker
import Universe.Adventure
import Player
import Actions
import Building
import Universe
import Util
import Worker
import Workplace
import Resources

buildBuildings :: MonadError String m => PlayerId -> Position -> Direction -> BuildingDescription -> Universe -> m Universe
buildBuildings plId pos dir buildings = performInteraction plId (BuildBuildingsInteraction buildings) $
  const $ const $ mapMOf (players . ix plId . buildingSpace) (buildNewBuildings pos dir buildings)

alterOccupants :: MonadError String m => PlayerId -> BuildingOccupants -> Universe -> m Universe
alterOccupants plId occupants universe = return $ universe &
  players . ix plId . buildingSpace . buildingSpaceOccupants .~ occupants

startWorking :: MonadError String m => PlayerId -> WorkerId -> WorkplaceId -> Universe -> m Universe
startWorking plId workerId workplaceId universe = do
  currentWorkplaceType <- checkMaybe "Invalid workplace" (universe ^? availableWorkplaces . ix workplaceId . workplaceType)
  check "Workplace already occupied" (workplaceId `elem` freeWorkplaces universe)
  playerData <- checkMaybe "Invalid player" (universe ^? players . ix plId)
  workerData <- checkMaybe "Invalid wrker" (playerData ^? workers . ix workerId)
  check "Worker already working" $ hasn't (currentWorkplace . traverse) workerData
  check "Not moving worker" (has (players . ix plId . playerStatus . filtered (== MovingWorker)) universe)
  checkOccupants universe plId
  let currentWorkplaceAction = workplaceAction currentWorkplaceType
  check "Precondition not met" $ actionPrecondition plId workerId workplaceId universe currentWorkplaceAction
  let universeWithMovedWorker = universe & players . ix plId . workers . ix workerId . currentWorkplace .~ Just workplaceId
  case currentWorkplaceAction of
    StepsAction steps -> universeWithMovedWorker &
      performSteps steps plId workplaceId <&>
      startNextPlayer plId
    CompositeAction composite -> return $ universeWithMovedWorker &
      players . ix plId . playerStatus .~ PerformingAction workplaceId composite

finishTurn :: MonadError String m => Universe -> m Universe
finishTurn universe = do
  check "Not every player finished" $ hasn't (players . traverse . playerStatus . filtered (/= Waiting)) universe
  let startPlayer = universe ^. startingPlayer
  return $ universe &
    players . traverse . workers . traverse . currentWorkplace .~ Nothing &
    players . traverse %~ collectCrops &
    players . ix startPlayer . playerStatus .~ MovingWorker &
    availableWorkplaces . traverse %~ addWorkplaceResources

type CropToPlant = (CropType, Position)

plantCrops :: MonadError String m => PlayerId -> [CropToPlant] -> Universe -> m Universe
plantCrops plId crops = performInteraction plId PlantCropsInteraction $ const $ const $ \universe -> do
  let groupedCrops = groupBy ((==) `on` fst) $ sortOn fst crops
      oldPlayerResources = fromMaybe zeroV $ universe ^? players . ix plId . playerResources
      subtractGroupResources resources grp = resources & cropResource (fst (head grp)) -~ length grp
      newPlayerResources = foldl' subtractGroupResources oldPlayerResources groupedCrops
  check "Too many crops" (all ((<=2) . length) groupedCrops)
  check "Not enough crops" $ isNonNegative newPlayerResources
  universe &
    mapMOf (players . ix plId . buildingSpace) (plantCropsInBuildingSpace crops) <&>
    players . ix plId . playerResources .~ newPlayerResources

collectResources :: MonadError String m => PlayerId -> Universe -> m Universe
collectResources plId = performInteraction plId CollectResourcesInteraction $ \workplaceId _ universe ->
  performStep CollectResourcesStep plId workplaceId universe

finishAction :: MonadError String m => PlayerId -> Universe -> m Universe
finishAction plId universe = do
  check "Cannot finish action right now" $ canFinishAction universe plId
  return $ startNextPlayer plId universe

hireWorker :: MonadError String m => PlayerId -> Universe -> m Universe
hireWorker plId = performInteraction plId HireWorkerInteraction $ \workplaceId _ universe ->
  let addedWorkerId = newWorkerId universe
  in return $ universe &
       players . ix plId . workers . at addedWorkerId . non initialWorkerState . currentWorkplace .~ Just workplaceId &
       players . ix plId . buildingSpace %~ findSpaceForWorker (WorkerOccupant addedWorkerId)

armWorker :: MonadError String m => PlayerId -> Int -> Universe -> m Universe
armWorker plId amount = performInteraction plId ArmWorkerInteraction $ \_ workerId universe -> do
  check "Cannot arm with more than 8" $ amount <= 8
  check "Need to arm with at least 1" $ amount >= 1
  check "Not enough iron" $ all (amount <=) $ toListOf (players . ix plId . playerResources . ironAmount) universe
  return $ universe &
    players . ix plId . workers . ix workerId . workerStrength +~ amount &
    players . ix plId . playerResources . ironAmount -~ amount

adventure :: MonadError String m => PlayerId -> AdventureReward -> Universe -> m Universe
adventure plId reward = performInteractionWithNewInteraction plId AdventureInteraction $ \workplaceId workerId universe -> do
  newActionDefinition <- case rewardInteraction reward
    of Just def -> do
         check "Cannot choose such reward" $ actionPrecondition plId workerId workplaceId universe (CompositeAction def)
         return $ Just $ def
       Nothing -> return Nothing
  return $ (over (players . ix plId) (applyReward reward universe) universe, newActionDefinition)

buildBarn :: MonadError String m => PlayerId -> Position -> Universe -> m Universe
buildBarn plId position = performInteraction plId BuildBarnInteraction $ \_ _ universe -> do
  let spaceOptic :: Traversal' Universe BuildingSpace
      spaceOptic = players . ix plId . buildingSpace
  check "Barn already there" $
    hasn't (spaceOptic . buildingSpaceBarns . traverse . filtered (== position)) universe
  check "Cannot build barn there" $
    allOf
      (spaceOptic . buildingSpaceBuildings . traverse . filtered ((position `elem`) . buildingPositions))
      canHaveBarn
      universe
  check "Already has two barns" $ hasn't (spaceOptic . buildingSpaceBarns . filtered ((>=2) . length)) universe
  return $ over (spaceOptic . buildingSpaceBarns) (position :) universe

performInteraction :: MonadError String m => PlayerId -> ActionInteraction -> (WorkplaceId -> WorkerId -> Universe -> m Universe) -> Universe -> m Universe
performInteraction plId int effect = performInteractionWithNewInteraction plId int $ \wpId wId u -> fmap (, Nothing) (effect wpId wId u)

performInteractionWithNewInteraction :: MonadError String m =>
                                        PlayerId ->
                                        ActionInteraction ->
                                        (WorkplaceId -> WorkerId -> Universe -> m (Universe, Maybe CompositeActionDefinition)) ->
                                        Universe ->
                                        m Universe
performInteractionWithNewInteraction plId interaction effect universe = do
  (workplaceId, playerStatusAction) <- checkMaybe "Not current player" $ universe ^? players . ix plId . playerStatus . statusActionAndWorkplace
  checkOccupants universe plId
  workerId <- checkMaybe "This shouldn't happen" -- TODO find a way to represent it in the type system
    $ universe ^? players . traverse . workers . to M.toList . traverse . filtered (has $ _2 . inWorkplace workplaceId) . _1
  check "Precondition not met" $ interactionPrecondition workerId workplaceId plId universe interaction
  let performStepsAndUpdateAction act steps u =
        u &
          performSteps steps plId workplaceId <&>
          players . ix plId . playerStatus .~ PerformingAction workplaceId act
  case actionAfterInteraction playerStatusAction interaction of
    InvalidInteraction -> throwError ("Not possible right now " ++ show interaction)
    ActionFinished steps -> do
      (universeWithEffect, maybeNewInteraction) <- effect workplaceId workerId universe
      case maybeNewInteraction of
        Nothing -> universeWithEffect &
                     performSteps steps plId workplaceId <&>
                     startNextPlayer plId
        Just act -> performStepsAndUpdateAction act steps universeWithEffect
    RemainingAction act steps -> do
      (universeWithEffect, maybeNewInteraction) <- effect workplaceId workerId universe
      let nextAction = case maybeNewInteraction of
            Nothing -> act
            Just prevAct -> ActionCombination AndThen prevAct act
      performStepsAndUpdateAction nextAction steps universeWithEffect

checkOccupants :: MonadError String m => Universe -> PlayerId -> m ()
checkOccupants universe plId = check "Fix occupants first" $ null $ getOccupantErrors universe plId
