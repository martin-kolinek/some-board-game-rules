{-# LANGUAGE TemplateHaskell #-}
module Resources where

import Control.Lens

data Resources = Resources {
  _woodAmount :: Int,
  _stoneAmount :: Int,
  _goldAmount :: Int,
  _ironAmount :: Int,
  _wheatAmount :: Int,
  _potatoAmount :: Int,
  _dogAmount :: Int,
  _sheepAmount :: Int,
  _donkeyAmount :: Int,
  _pigAmount :: Int,
  _money :: Int,
  _foodAmount :: Int
} deriving (Show, Eq)

makeLenses ''Resources

initialResources :: Resources
initialResources = Resources 0 0 0 0 0 0 0 0 0 0 0 0

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

getDogAmount :: Resources -> Int
getDogAmount = _dogAmount

getSheepAmount :: Resources -> Int
getSheepAmount = _sheepAmount

getPigAmount :: Resources -> Int
getPigAmount = _pigAmount

getMoney :: Resources -> Int
getMoney = _money

getFoodAmount :: Resources -> Int
getFoodAmount = _foodAmount