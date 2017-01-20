module Decisions where

data WorkerNeedOptions = HireWorker | BuildRoom deriving (Eq, Show, Enum, Ord)

data CaveOrPassageOptions = ChooseCave | ChoosePassage | NoDigging deriving (Eq, Show, Enum, Ord)

data AnyRoomOptions = ChooseNoRoom | ChooseLivingRoom deriving (Eq, Show, Enum, Ord)

data Options =
  WorkerNeedOption WorkerNeedOptions |
  CaveOrPassageOption CaveOrPassageOptions |
  AnyRoomOption AnyRoomOptions deriving (Eq, Show, Ord)
