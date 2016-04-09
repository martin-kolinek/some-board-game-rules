{-# LANGUAGE TemplateHaskell #-}

module Worker where

import Workplace
import Control.Lens

newtype WorkerId = WorkerId Int deriving (Eq, Ord, Show)

data WorkerState = WorkerState {
  _currentWorkplace :: Maybe WorkplaceId
} deriving (Show, Eq)

initialWorkerState = WorkerState Nothing

makeLenses ''WorkerState
