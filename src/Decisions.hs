module Decisions where

import Player

data WorkerNeedOptions = HireWorker | BuildRoom deriving (Eq, Show, Enum)

data CaveOrPassageOptions = ChooseCave | ChoosePassage | NoDigging deriving (Eq, Show, Enum)

data AnyRoomOptions = ChooseNoRoom | ChooseLivingRoom deriving (Eq, Show, Enum)

data Options =
  WorkerNeedOption WorkerNeedOptions |
  CaveOrPassageOption CaveOrPassageOptions |
  AnyRoomOption AnyRoomOptions deriving (Eq, Show)

decisionOptions :: DecisionType -> [Options]
decisionOptions (WorkerNeedDecision _) = WorkerNeedOption <$> [HireWorker ..]
decisionOptions CaveOrPassageDecision = CaveOrPassageOption <$> [ChooseCave ..]
decisionOptions AnyRoomDecision = AnyRoomOption <$> [ChooseNoRoom ..]
