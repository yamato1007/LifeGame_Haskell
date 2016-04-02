module Main where 

import LifeGame.Lifegame 
import LifeGame.Board
import Util

import System.Random

--定数
sizeX = 30
sizeY = 24
probability = 0.3
generation = 100


main = do
  gen <- getStdGen
  let board = randomBoard (sizeX,sizeY) probability gen
  lifeGame generation board

