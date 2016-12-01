module Rules (
  -- * Types
  -- ** Main universe type
  Universe,
  -- ** Identifier types
  WorkerId,
  WorkplaceId,
  PlayerId,
  DogId,
  -- ** Workplaces
  WorkplaceData(..),
  -- ** Resources
  Resources,
  -- ** Buildings
  Building(..),
  BuildingOccupant(..),
  BuildingOccupants,
  Direction(..),
  Position,
  OccupantError,
  -- ** Decisions
  DecisionType (..),
  Options (..),
  WorkerNeedOptions(..),
  CaveOrPassageOptions(..),
  AnyRoomOptions(..),

  -- * Access functions
  -- ** Players
  getPlayers,
  getCurrentPlayer,
  getStartingPlayer,
  -- ** Workers
  getWorkers,
  getWorkplaces,
  getWorkerWorkplace,
  getWorkplaceOccupants,
  -- ** Buildings
  getBuildingSpace,
  getBuildingOccupants,
  getAllOccupants,
  getOccupantErrors,
  -- ** Resources
  getPlayerResources,
  getWoodAmount,
  getStoneAmount,
  getGoldAmount,
  getIronAmount,
  getWheatAmount,
  getPotatoAmount,
  getMoney,
  getFoodAmount,
  getDogs,
  -- ** Player status functions
  getPossibleDecisions,
  isMovingWorker,
  isPlantingCrops,
  isSelectingPosition,

  -- * Action functions
  startWorking,
  finishTurn,
  selectPosition,
  alterOccupants,
  cancelSelection,
  chooseOption,
  plantCrops,

  -- * Starting state
  initialUniverse,

  -- * Utility functions
  directionAddition,
  allDirections,
  availableBuildingPositions,
  ) where

import Worker
import Workplace
import Building
import Player
import Universe
import Resources
import Decisions
import Universe.Actions
import Universe.Player
import Universe.Workplace
import Universe.Worker
import Universe.Building
import Universe.Decisions
