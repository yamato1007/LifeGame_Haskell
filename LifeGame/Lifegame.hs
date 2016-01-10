module LifeGame.Lifegame where

import LifeGame.Indiv
import LifeGame.Board
import qualified Util

--定数
--生きている個体が次世代でも生き残るには周りに何体生きている個体がいるか
survival = [2,3]
--死んでいる個体が次世代誕生するには、まわりに何体生きている個体がいるか
birth = [3]

--n世代分世代をすすめる
lifeGame :: Int -> Board -> IO()
lifeGame 0 _ = return ()
lifeGame n board = do
  dispBoard board
  lifeGame (n-1) (nextGeneration board)

--世代を一つすすめる
nextGeneration :: Board -> Board
nextGeneration board = Util.zipWithNest nextIndiv board $ Util.getNeighborList board

--現在の個体状態と隣接する個体のリストからその個体の次の状態を返す
nextIndiv :: Indiv -> [Indiv] -> Indiv
nextIndiv Life neibor
  |countLife neibor `elem` survival = Life
  |otherwise = Dead
nextIndiv Dead neibor
  |countLife neibor `elem` birth = Life
  |otherwise = Dead


