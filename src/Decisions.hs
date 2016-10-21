module Decisions where

import Player

data WorkerNeedOptions = HireWorker | BuildRoom deriving (Eq, Show, Enum, Ord)

data CaveOrPassageOptions = ChooseCave | ChoosePassage | NoDigging deriving (Eq, Show, Enum, Ord)

data AnyRoomOptions = ChooseNoRoom | ChooseLivingRoom deriving (Eq, Show, Enum, Ord)

data Options =
  WorkerNeedOption WorkerNeedOptions |
  CaveOrPassageOption CaveOrPassageOptions |
  AnyRoomOption AnyRoomOptions deriving (Eq, Show, Ord)

decisionOptions :: DecisionType -> [Options]
decisionOptions (WorkerNeedDecision _) = WorkerNeedOption <$> [HireWorker ..]
decisionOptions CaveOrPassageDecision = CaveOrPassageOption <$> [ChooseCave ..]
decisionOptions AnyRoomDecision = AnyRoomOption <$> [ChooseNoRoom ..]
