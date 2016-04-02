module Util where


import Control.Monad 

--二次元ベクトル
type Vector a = (a,a)

--二次元正数ベクトル
type Point = Vector Int
 
--長いリストを指定した長さからなる短いリストに分割する
separate :: Int -> [a] -> [[a]]
separate 0 _ = []
separate _ [] = []
separate n list = (take n list) : (separate n $ drop n list)

--ネストしたリストから指定した座標の要素を得る
infixl 9 !!!
(!!!) :: [[a]] -> Point -> Maybe a
(!!!) list (x,y)
  |x < 0 || y < 0 = Nothing
  |y >= length list = Nothing
  |x >= length (list !! y) = Nothing
  |otherwise = Just $ (list !! y) !! x

--ネストしたリストから、指定した座標に隣接する要素のリストを返す
getNeighbor :: Eq a => Point -> [[a]] -> [a]
getNeighbor _ [] = []
getNeighbor (x,y) list =
  putOutMaybe $ do 
    x' <- [x-1 .. x+1]
    y' <- [y-1 .. y+1]
    guard ((x',y') /= (x,y))
    return $ list !!! (x',y')

--ネストしたリストから、各要素に隣接する要素のリストからなるネストしたリストを返す
getNeighborList :: (Eq a) => [[a]] -> [[[a]]]
getNeighborList [] = []
getNeighborList [[]] = [[]]
getNeighborList list = column 0
  where column y
          |y == (length list) = []
          |otherwise = row 0 y : column (y+1)
        row x y
          |x == (length (list !! y)) = []
          |otherwise = getNeighbor (x,y) list : row (x+1) y

--MaybeのリストからNothingを除き、中の値を取り出したリストを返す
putOutMaybe :: Eq a => [Maybe a] -> [a]
putOutMaybe [] = []
putOutMaybe xs = do 
  Just x <- xs
  return x
          
--リストから条件が真となる要素の数を数える
countf :: (a -> Bool) -> [a] -> Int
countf _ [] = 0
countf f list = foldl (\acc x -> if f x then acc+1 else acc) 0 list

--リストが指定の要素をいくつ含むかカウントする
count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x list = countf (==x) list 

--ネストしたリストに適応できるmap関数
mapNest :: (a -> b) -> [[a]] -> [[b]]
mapNest f = map (map f)

--ネストしたリストに適用できるzipWith関数
zipWithNest :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWithNest f l1 l2 = map (\(x1,x2) -> zipWith f x1 x2) $ zip l1 l2  
