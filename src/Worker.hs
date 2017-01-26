{-# LANGUAGE TemplateHaskell #-}

module Worker where

import Workplace

import Control.Lens
import Data.Map.Strict

newtype WorkerId = WorkerId Int deriving (Eq, Ord, Show)

type WorkerStrength = Int

data WorkerState = WorkerState {
  _currentWorkplace :: Maybe WorkplaceId
} deriving (Show, Eq)

initialWorkerState :: WorkerState
initialWorkerState = WorkerState Nothing

makeLenses ''WorkerState

createWorkers :: Int -> Int -> Map WorkerId WorkerState
createWorkers initial count = fromList [(WorkerId i, initialWorkerState) | i <- [initial .. initial + count - 1]]
