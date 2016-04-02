module LifeGame.Indiv where

import System.Random

--ライフゲームの個体
data Indiv = Life | Dead deriving (Eq,Ord,Enum)
instance Show Indiv where
  show Life = "■"
  show Dead = " "

--ランダムな個体を一つ得る
randomIndiv :: (RandomGen a) => a -> (Indiv, a)
randomIndiv gen = (if rand then Life else Dead, gen')
  where (rand, gen') = randomR (False, True) gen

--Bool値からIndivへの変換
boolToIndiv :: Bool -> Indiv
boolToIndiv True = Life
boolToIndiv False = Dead

