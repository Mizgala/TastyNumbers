module Misc where
import Numbers
import Data.List (genericLength)

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

numberFunction :: (Integer -> Integer -> Integer) -> Number -> Number -> Number
numberFunction f num1 num2 
    | (snd num1) /= (snd num2)  = ([],0)
    | otherwise                 = (merge f (fst num1) (fst num2), snd num1)

merge :: (Integer -> Integer -> Integer) -> [Integer] -> [Integer] -> [Integer]
merge _ [] []           = []
merge _ (x:[]) []       = [x]
merge _ [] (y:[])       = [y]
merge _ (x:xs) []       = x : xs
merge _ [] (y:ys)       = y : ys
merge f (x:xs) (y:ys)   = (f x y) : (merge f xs ys)

applyFuncs :: [a -> b] -> a -> [b]
applyFuncs [] _         = []
applyFuncs [f] num      = [f num]
applyFuncs (f:fs) num   = (f num : applyFuncs fs num)

listToBase10 :: [Integer] -> Integer
listToBase10 (num:[])   = num
listToBase10 nums       = ((head nums) * (10 ^ (genericLength nums - 1))) + (listToBase10 (tail nums))

numberToIntegers :: Number -> [Integer]
numberToIntegers (nums,base) = numberToIntegers' (base - 1) nums

numberToIntegers' :: Integer -> [Integer] -> [Integer]
numberToIntegers' num (dig:digs)
    | num == 0  = newReplicate dig 0
    | otherwise = (newReplicate dig num) ++ (numberToIntegers' (num-1) digs)
