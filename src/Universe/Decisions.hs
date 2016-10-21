module Universe.Decisions where

import Control.Lens hiding (universe)

import Universe.Player
import Universe
import Player
import Decisions

getPossibleDecisions :: Universe -> [Decisions.Options]
getPossibleDecisions universe = do
  plData <- universe ^.. currentPlayerData
  case plData ^. playerStatus of
    MakingDecision decisionType -> decisionOptions decisionType
    _ -> []
