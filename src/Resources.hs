{-# LANGUAGE TemplateHaskell, DeriveAnyClass #-}
module Resources where

import Control.Lens
import Data.AdditiveGroup
import Data.List

data Resources = Resources {
  _woodAmount :: Int,
  _stoneAmount :: Int,
  _goldAmount :: Int,
  _ironAmount :: Int,
  _wheatAmount :: Int,
  _potatoAmount :: Int,
  _moneyAmount :: Int,
  _foodAmount :: Int
} deriving (Eq)

makeLenses ''Resources

initialResources :: Resources
initialResources = zeroV

getWoodAmount :: Resources -> Int
getWoodAmount = _woodAmount

getStoneAmount :: Resources -> Int
getStoneAmount = _stoneAmount

getGoldAmount :: Resources -> Int
getGoldAmount = _goldAmount

getIronAmount :: Resources -> Int
getIronAmount = _ironAmount

getWheatAmount :: Resources -> Int
getWheatAmount = _wheatAmount

getPotatoAmount :: Resources -> Int
getPotatoAmount = _potatoAmount

getMoneyAmount :: Resources -> Int
getMoneyAmount = _moneyAmount

getFoodAmount :: Resources -> Int
getFoodAmount = _foodAmount

instance Show Resources where
  show res = if null nonZero then "NoResources" else "Resources {" ++ (intercalate ", " nonZero) ++ "}"
    where nonZero = display <$> filter ((> 0) . snd) (getPair <$> [("wood", getWoodAmount),
                                                                   ("stone", getStoneAmount),
                                                                   ("gold", getGoldAmount),
                                                                   ("iron", getIronAmount),
                                                                   ("wheat", getWheatAmount),
                                                                   ("potato", getPotatoAmount),
                                                                   ("money", getMoneyAmount),
                                                                   ("food", getFoodAmount)])
          display (name, value) = name ++ " = " ++ show value
          getPair (name, getter)= (name, getter res)

resourcesToTuple :: Resources -> ((Int, Int, Int, Int), (Int, Int, Int, Int))
resourcesToTuple (Resources wd st gld ir wh pot mon fd) = ((wd, st, gld, ir), (wh, pot, mon, fd))
tupleToResources :: ((Int, Int, Int, Int), (Int, Int, Int, Int)) -> Resources
tupleToResources ((wd, st, gld, ir), (wh, pot, mon, fd)) = (Resources wd st gld ir wh pot mon fd)

instance AdditiveGroup Resources where
  zeroV = tupleToResources zeroV
  r1 ^+^ r2 = tupleToResources (resourcesToTuple r1 ^+^ resourcesToTuple r2)
  negateV = tupleToResources . negateV . resourcesToTuple

wood :: Int -> Resources
wood x = Resources x 0 0 0 0 0 0 0

stone :: Int -> Resources
stone x = Resources 0 x 0 0 0 0 0 0

gold :: Int -> Resources
gold x = Resources 0 0 x 0 0 0 0 0

iron :: Int -> Resources
iron x = Resources 0 0 0 x 0 0 0 0

wheat :: Int -> Resources
wheat x = Resources 0 0 0 0 x 0 0 0

potato :: Int -> Resources
potato x = Resources 0 0 0 0 0 x 0 0

money :: Int -> Resources
money x = Resources 0 0 0 0 0 0 x 0

food :: Int -> Resources
food x = Resources 0 0 0 0 0 0 0 x

isNonNegative :: Resources -> Bool
isNonNegative (Resources wd st gld ir wh pot mon fd) =
  wd >= 0 &&
  st >= 0 &&
  gld >= 0 &&
  ir >= 0 &&
  wh >= 0 &&
  pot >= 0 &&
  mon >= 0 &&
  fd >= 0

newtype AnimalId = AnimalId Int deriving (Show, Eq, Ord)

data FarmAnimalType = Sheep | Cow deriving (Show, Eq, Ord)

data AnimalType = Dog | FarmAnimalType FarmAnimalType deriving (Show, Eq, Ord)

data Animal = Animal AnimalType AnimalId deriving (Show, Eq, Ord)
