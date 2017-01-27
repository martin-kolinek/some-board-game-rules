{-# LANGUAGE TemplateHaskell #-}

module Worker where

import Workplace

import Control.Lens
import Data.Map.Strict

newtype WorkerId = WorkerId Int deriving (Eq, Ord, Show)

type WorkerStrength = Int

data WorkerState = WorkerState {
  _currentWorkplace :: Maybe WorkplaceId,
  _workerStrength :: WorkerStrength
} deriving (Show, Eq)

initialWorkerState :: WorkerState
initialWorkerState = WorkerState Nothing 0

makeLenses ''WorkerState

createWorkers :: Int -> Int -> Map WorkerId WorkerState
createWorkers initial count = fromList [(WorkerId i, initialWorkerState) | i <- [initial .. initial + count - 1]]
