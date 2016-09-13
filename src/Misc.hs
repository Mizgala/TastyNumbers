{-# OPTIONS_GHC -Wall #-}
module Misc where

newTake :: Integer -> [a] -> [a]
newTake 0 _         = []
newTake _ []        = []
newTake count list  = head list : newTake (count - 1) (tail list)

newReplicate :: Integer -> a -> [a]
newReplicate count thing = ((newTake count) . repeat) thing

modList :: [Integer] -> [[Integer]] -> [[Integer]]
modList [] _ = []
modList (_:(_:_)) [] = []
modList [_] [] = []
modList (x:[]) (y:[]) = [map (+x) y]
modList (x:xs) (y:ys) = map (+x) y : modList xs ys
