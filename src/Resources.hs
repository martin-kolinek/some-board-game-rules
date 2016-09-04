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

getWoodAmount = _woodAmount

getStoneAmount = _stoneAmount

getGoldAmount = _goldAmount

getIronAmount = _ironAmount

getWheatAmount = _wheatAmount

getPotatoAmount = _potatoAmount

getDogAmount = _dogAmount

getSheepAmount = _sheepAmount

getPigAmount = _pigAmount

getMoney = _money

getFoodAmount = _foodAmount