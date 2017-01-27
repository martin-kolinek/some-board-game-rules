module Decisions where

import Worker

data WorkerNeedOptions = HireWorker | BuildRoom deriving (Eq, Show, Enum, Ord)

data CaveOrPassageOptions = ChooseCave | ChoosePassage | NoDigging deriving (Eq, Show, Enum, Ord)

data ArmOptions = ArmWorker WorkerStrength | NoArming deriving (Eq, Show, Ord)

data Options =
  WorkerNeedOption WorkerNeedOptions |
  CaveOrPassageOption CaveOrPassageOptions |
  ArmOption ArmOptions
  deriving (Eq, Show, Ord)
