module Main where 

import LifeGame.Lifegame 
import LifeGame.Board
import Util

import System.Random

--定数
sizeX = 40
sizeY = 25
probability = 0.3
generation = 200


main = do
  gen <- getStdGen
  let board = randomBoard (sizeX,sizeY) probability gen

  lifeGame generation board

