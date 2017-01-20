module Universe.Decisions where

import Control.Lens hiding (universe)

import Universe
import Player
import Decisions
import Actions

getPossibleDecisions :: Universe -> PlayerId -> [Decisions.Options]
getPossibleDecisions universe plId = do
  plData <- universe ^.. (players . ix plId)
  case plData ^. playerStatus of
    PerformingAction _ (Decision options) -> fst <$> options
    _ -> []
