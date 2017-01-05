module Universe.Decisions where

import Control.Lens hiding (universe)
import Data.List.NonEmpty (NonEmpty(..))

import Universe
import Player
import Decisions

getPossibleDecisions :: Universe -> PlayerId -> [Decisions.Options]
getPossibleDecisions universe plId = do
  plData <- universe ^.. (players . ix plId)
  case plData ^. playerStatus of
    PerformingAction (MakingDecision decisionType :| _) -> decisionOptions decisionType
    _ -> []
