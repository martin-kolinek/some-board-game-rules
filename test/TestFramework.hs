{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module TestFramework where

import Rules

import Control.Monad.State
import Control.Monad.Except
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Generators
import Text.Show.Pretty
import Data.Maybe
import Data.Map (empty, insert)
import Data.Either (isLeft)

type UniversePropertyMonad = PropertyM (State (Either String Universe))

newtype ArbitraryUniverse = ArbitraryUniverse Universe

instance Arbitrary ArbitraryUniverse where
  arbitrary = ArbitraryUniverse <$> generateUniverse defaultGeneratorProperties
  shrink (ArbitraryUniverse u) = ArbitraryUniverse <$> shrinkUniverse u

instance Show ArbitraryUniverse where
  show (ArbitraryUniverse u) = ppShow u

defaultGeneratorProperties :: GeneratorProperties
defaultGeneratorProperties = GeneratorProperties empty defaultInteractionProbabilities 1 5 5 5 defaultStepProbabilities

defaultInteractionProbabilities :: InteractionProbabilities
defaultInteractionProbabilities = InteractionProbabilities 1 1 1 1 1 1 1

defaultStepProbabilities :: StepProbabilities
defaultStepProbabilities = StepProbabilities 1 1 1 1 1

withWorkplaceProbability :: WorkplaceType -> Int -> GeneratorProperties -> GeneratorProperties
withWorkplaceProbability wpType probability props = props { workplaceProbabilities = insert wpType probability (workplaceProbabilities props) }

withFarmingProbability :: Int -> GeneratorProperties -> GeneratorProperties
withFarmingProbability probability props = props { interactionProbabilities = (interactionProbabilities props) { plantCropsProbability = probability}}

withArmingProbability :: Int -> GeneratorProperties -> GeneratorProperties
withArmingProbability probability props = props { interactionProbabilities = (interactionProbabilities props) { armWorkerProbability = probability}}

withAdventureProbability :: Int -> GeneratorProperties -> GeneratorProperties
withAdventureProbability probability props = props { interactionProbabilities = (interactionProbabilities props) { adventureProbability = probability}}

withNoResourceChangeSteps :: GeneratorProperties -> GeneratorProperties
withNoResourceChangeSteps props = props {stepProbabilities = (stepProbabilities props) {
                                            addResourcesProbability = 0,
                                            collectResourcesStepProbability = 0,
                                            payResourcesProbability = 0,
                                            addDogProbability = 0}}

withOtherWorkersNotDoneProbability :: Int -> GeneratorProperties -> GeneratorProperties
withOtherWorkersNotDoneProbability probability props = props { otherWorkersNotDoneProbability = probability }

generalUniverseProperty :: Testable a => UniversePropertyMonad a -> Property
generalUniverseProperty = propertyWithProperties defaultGeneratorProperties

movingWorkerProperty :: Testable a => UniversePropertyMonad a -> Property
movingWorkerProperty = propertyWithProperties defaultGeneratorProperties {movingWorkerProbability = 50}

newtype PrettyPrintedUniverse = PrettyUniverse { unPretty :: Universe}

instance Show PrettyPrintedUniverse where
  show (PrettyUniverse u) = ppShow u

propertyWithProperties :: Testable a => GeneratorProperties -> UniversePropertyMonad a -> Property
propertyWithProperties properties action = monadic extractProperty action
  where extractProperty act = forAllShrink (PrettyUniverse <$> generateUniverse properties) ((fmap PrettyUniverse) . shrinkUniverse . unPretty) (execOnUniverse act)
        execOnUniverse act (PrettyUniverse universe) = checkResult $ runState act (Right universe)
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
  when (isLeft nextState) $ monitor $ counterexample ("Last valid universe: " ++ ppShow currentState)
  run $ put nextState

shouldHaveFailed :: UniversePropertyMonad ()
shouldHaveFailed = do
  universeOrError <- run get
  case universeOrError of
    Right universe -> stop $ counterexample ("Was expecting failure, but was successful: " ++ ppShow universe) False
    Left _ -> return ()

preMaybe :: Monad m => Maybe b -> PropertyM m b
preMaybe value = pre (isJust value) >> return (fromJust value)

