{-# LANGUAGE TemplateHaskell #-}

module Universe where

import           Control.Lens
import           Control.Lens.TH

data Universe = Universe {
  _availableWorkplaces :: [Workplace],
  _allWorkers          :: [Worker]
}

data Worker = Worker {
  _currentWorkplace :: Maybe Workplace
}

type UniverseAction = Universe -> Universe

data Workplace = Workplace {
  _action :: UniverseAction
}

makeLenses ''Universe
makeLenses ''Workplace
