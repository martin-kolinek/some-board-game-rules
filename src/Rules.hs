module Rules (
  -- * Types
  -- ** Main universe type
  Universe,
  -- ** Identifier types
  WorkerId,
  WorkplaceId,
  PlayerId,
  AnimalId,
  -- ** Workers
  WorkerStrength,
  -- ** Workplaces
  WorkplaceData,
  WorkplaceType(..),
  -- ** Resources
  Resources,
  -- ** Animals
  Animal (..),
  AnimalType (..),
  FarmAnimalType (..),
  -- ** Buildings
  Building(..),
  SmallBuildingType(..),
  LargeBuildingType(..),
  BuildingDescription(..),
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
  -- ** Adventure
  AdventureReward(..),

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
  getBarns,
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
  getAnimals,
  -- ** Player status functions
  isMovingWorker,
  isPlantingCrops,
  currentlyBuiltBuildings,
  canCollectResources,
  canHireWorker,
  canFinishAction,
  isArmingWorker,
  canGoOnAdventure,

  -- * Action functions
  startWorking,
  collectResources,
  buildBuildings,
  plantCrops,
  finishAction,
  hireWorker,
  alterOccupants,
  armWorker,
  adventure,

  finishTurn,

  -- * Starting state
  initialUniverse,

  -- * Utility functions
  directionAddition,
  allDirections,
  availableBuildingPositions,
  buildingPositions
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
import Universe.Adventure
