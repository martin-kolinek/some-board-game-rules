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
  WorkplaceData,
  WorkplaceType(..),
  -- ** Resources
  Resources,
  -- ** Buildings
  Building(Building),
  BuildingType(..),
  BuildingOccupant(..),
  BuildingOccupants,
  Direction(..),
  Position,
  OccupantError,
  CropToPlant,
  CropType(..),
  PlantedCrop(..),
  -- ** Decisions
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
  getWorkerWorkplace,
  getWorkplaceOccupants,
  -- ** Workplaces
  getWorkplaces,
  getWorkplaceType,
  getWorkplaceResources,
  -- ** Buildings
  getBuildingSpace,
  getBuildingOccupants,
  getAllOccupants,
  getOccupantErrors,
  getPlantedCrops,
  -- ** Resources
  getPlayerResources,
  getWoodAmount,
  getStoneAmount,
  getGoldAmount,
  getIronAmount,
  getWheatAmount,
  getPotatoAmount,
  getMoneyAmount,
  getFoodAmount,
  getDogs,
  -- ** Player status functions
  getPossibleDecisions,
  isMovingWorker,
  isPlantingCrops,
  canCancelBuilding,
  currentlyBuiltBuildings,

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
