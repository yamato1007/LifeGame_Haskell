module Main where 

import LifeGame.Lifegame 
import LifeGame.Board
import System.Random

import Util

--定数
sizeX = 30
sizeY = 30



main = do
  gen <- getStdGen
  let board = randomBoard (sizeX,sizeY) 0.01 gen

  lifeGame 100 board

