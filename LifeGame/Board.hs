module LifeGame.Board where

import System.Random
import LifeGame.Indiv
import qualified Util

--ライフゲームのボード
type Board = [[Indiv]]

--ライフゲームのボードを見やすく文字列化　
boardToString :: Board -> String
boardToString board = 
  bar ++ '\n':show' board ++ bar
    where bar = '+' : replicate (length $ head board) '-' ++ "+"
          show' [] = []
          show' (x:xs) = '|':(show'' x) ++ '\n' :(show' xs)
            where show'' [] = "|"
                  show'' (x:xs) = show x ++ show'' xs

--ライフゲームのボードの表示
dispBoard :: Board -> IO()
dispBoard board = putStrLn $ boardToString board 


--空のライフゲームボードを作成
mkBoard :: Util.Point -> Board
mkBoard (x, y) = replicate x $ replicate y Dead

--指定した確率で生存個体の存在するライフゲームボードを作成
randomBoard :: (RandomGen a) => Util.Point -> Float -> a -> Board
randomBoard (x,y) p gen = Util.separate x $ map boolToIndiv $ map (<=p) $ take (x*y) $ randomRs (0.0,1.0) gen

--隣接した個体のリストからなるBoardを生成
getNeighborBoard :: Board -> [[[Indiv]]]
getNeighborBoard board = Util.getNeighborList board

--ボード上のある場所から隣接する個体のリストを得る
getNeighbor :: Util.Point -> Board -> [Indiv]
getNeighbor p b = Util.getNeighbor p b

--ボードから指定のIndivの数をカウント
countIndivBoard :: Indiv -> Board -> Int 
countIndivBoard i board = sum $ map (countIndiv i) board
--ボードから生きている個体の数をカウント
countLifeBoard = countIndivBoard Life
--ボードから死んでる個体の数をカウント
countDeadBoard = countIndivBoard Dead

--リストから指定のIndivの数をカウント
countIndiv :: Indiv -> [Indiv] -> Int
countIndiv i list = Util.count i list
--リストから生きている個体の数をカウント
countLife = countIndiv Life
--リストから死んでる個体をカウント
countDead = countIndiv Dead
