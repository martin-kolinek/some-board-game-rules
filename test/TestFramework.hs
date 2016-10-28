{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module TestFramework where

import Rules

import Test.Tasty
import Test.Tasty.HUnit as H hiding (assert)
import Control.Monad.State
import Data.Map.Strict hiding (null)
import qualified Data.Set as S
import Control.Monad.Except
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Generators
import Text.Show.Pretty
import Data.Maybe

type FlowTest t = ExceptT String (StateT t IO)

flowTestCase :: a -> String -> FlowTest a () -> TestTree
flowTestCase = flowTestCaseWithCheck check
  where check (Left msg) = assertFailure $ "No error expected, but got: " ++ msg
        check _ = return ()

flowTestCaseFailure :: a -> String -> FlowTest a () -> TestTree
flowTestCaseFailure = flowTestCaseWithCheck check
  where check (Right _) = assertFailure "Expected error, but got none"
        check _ = return ()

flowTestCaseWithCheck :: (Either String a -> IO ()) -> u -> TestName -> FlowTest u a -> TestTree
flowTestCaseWithCheck check initial name action = testCase name $ do
  (result, _) <- runStateT (runExceptT action) initial
  check result
  return ()

getPlayer :: Int -> FlowTest Universe PlayerId
getPlayer number = (!! number) . getPlayers <$> get

player1 :: FlowTest Universe PlayerId
player1 = getPlayer 0

player2 :: FlowTest Universe PlayerId
player2 = getPlayer 1

getWorker :: Int -> Int -> FlowTest Universe WorkerId
getWorker playerNumber workerNumber = (!! workerNumber) <$> (getWorkers <$> get <*> getPlayer playerNumber)

getWorkplace :: MonadState Universe f => Int -> f WorkplaceId
getWorkplace number = (!! number) . keys . getWorkplaces <$> get

apply :: (forall m. MonadError String m => (a -> m a)) -> FlowTest a ()
apply action = put =<< action =<< get

type UniversePropertyMonad = PropertyM (State (Either String Universe))

universeProperty :: UniversePropertyMonad a -> ArbitraryUniverse -> Property
universeProperty action (ArbitraryUniverse universe) = monadic extractProperty action
  where extractProperty act = checkResult $ runState act (Right universe)
        checkResult (prop, Right resultUniverse) =
          counterexample ("Resulting universe: " ++ ppShow resultUniverse) $
          prop
        checkResult (prop, Left msg) =
          counterexample ("Unexpected error: " ++ msg) $
          prop

getUniverse :: UniversePropertyMonad Universe
getUniverse = do
  universeOrError <- run get
  case universeOrError of
    Right universe -> return universe
    Left msg -> stop $ counterexample ("Tried to access universe, but was already failed: " ++ msg) False

getsUniverse :: (Universe -> a) -> UniversePropertyMonad a
getsUniverse x = x <$> getUniverse

applyToUniverse :: (forall m. MonadError String m => Universe -> m Universe) -> UniversePropertyMonad ()
applyToUniverse action = do
  currentState <- run get
  let nextState = action =<< currentState
  run $ put nextState

shouldHaveFailed :: UniversePropertyMonad ()
shouldHaveFailed = do
  universeOrError <- run get
  case universeOrError of
    Right universe -> stop $ counterexample ("Was expecting failure, but was successful: " ++ ppShow universe) False
    Left _ -> return ()

preMaybe :: Monad m => Maybe b -> PropertyM m b
preMaybe value = pre (isJust value) >> return (fromJust value)

startWorkingInWorkplaceType :: (WorkplaceData -> Bool) -> UniversePropertyMonad (PlayerId, WorkerId, WorkplaceId)
startWorkingInWorkplaceType typeFilter = do
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
                typeFilter workplaceData && not (S.member workplaceId filledWorkplaceIds)
              filledWorkplaceIds = S.fromList (extractFilledWorkplaces universe)
  currentPlayerId <- preMaybe =<< getsUniverse getCurrentPlayer
  pre =<< getsUniverse isMovingWorker <*> pure currentPlayerId
  freeWorkers <- getsUniverse $ extractFreeWorkers currentPlayerId
  pre $ not $ null $ freeWorkers
  selectedWorkerId <- pick $ elements freeWorkers
  appropriateWorkplaces <- getsUniverse extractAppropriateWorkpalces
  pre $ not $ null $ appropriateWorkplaces
  selectedWorkplaceId <- pick $ elements $ keys appropriateWorkplaces
  applyToUniverse $ startWorking selectedWorkerId selectedWorkplaceId
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

selectWrongPosition :: (Universe -> PlayerId -> [(Position, Direction)]) -> PlayerId -> UniversePropertyMonad (Position, Direction)
selectWrongPosition func plId = do
  (pos, dir) <- pickWrongPosition func plId
  applyToUniverse $ selectPosition pos dir
  return (pos, dir)

selectCorrectPosition :: (Universe -> PlayerId -> [(Position, Direction)]) -> PlayerId -> UniversePropertyMonad (Position, Direction)
selectCorrectPosition func plId = do
  (pos, dir) <- pickSpecificPosition func plId
  applyToUniverse $ selectPosition pos dir
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

validatePlayerHasValidOccupants :: PlayerId -> UniversePropertyMonad ()
validatePlayerHasValidOccupants plId = assert =<< null <$> (getsUniverse getOccupantErrors <*> pure plId)
