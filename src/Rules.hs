module Rules (
  -- * Types
  -- ** Main universe type
  Universe,
  -- ** Identifier types
  WorkerId,
  WorkplaceId,
  PlayerId,
  DogId,
  -- ** Workers
  WorkerStrength,
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
  ArmOptions(..),

  -- * Access functions
  -- ** Players
  getPlayers,
  getCurrentPlayer,
  getStartingPlayer,
  -- ** Workers
  getWorkers,
  getWorkerWorkplace,
  getWorkplaceOccupants,
  getWorkerStrength,
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
  isMovingWorker,
  isPlantingCrops,
  canCancelBuilding,
  currentlyBuiltBuildings,

  -- * Action functions
  startWorking,
  buildBuildings,
  alterOccupants,
  plantCrops,
  collectResources,
  finishAction,
  hireWorker,

  finishTurn,

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
import Universe.Interaction
import Universe.Player
import Universe.Workplace
import Universe.Worker
import Universe.Building
