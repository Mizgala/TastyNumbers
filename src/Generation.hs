{-# OPTIONS_GHC -Wall #-}
module Generation where
import Data.List(genericLength, genericReplicate)
import Numbers
import Misc

{-  This is a set of functions used to generate candidates to look for Tasty
 -  numbers.-}

maxCandidate :: Integer -> Integer -> [Integer]
maxCandidate base freq = map (* freq) (newReplicate base 1) 

candRange :: Integer -> Integer -> [[Integer]]
candRange base freq = map (range 0) (maxCandidate base freq)

range :: Integer -> Integer -> [Integer]
range start end = [start..end]

combos :: [[Integer]] -> [[Integer]]
combos []   = [[]]
combos ([]:ls)  = combos ls
combos ((h:t):ls) = map (h:) (combos ls) ++ combos (t:ls)

lengthCheck :: Integer -> [Integer] -> Bool
lengthCheck base list = base == (genericLength list)

genCands' :: Integer -> Integer -> [[Integer]]
genCands' base freq = (filter (lengthCheck base) . combos . (candRange base)) freq

genCands :: Integer -> Integer -> [Number]
genCands base freq = map (makeNumber base) (genCands' base freq)

makeSet :: Integer -> [Integer] -> [Integer]
makeSet _ [] = []
makeSet start (num:[]) = newReplicate num start
makeSet start (num:nums) = (newReplicate num start) ++ (makeSet (start + 1) nums)

genCands2 :: Integer -> Integer -> Integer -> [Number]
genCands2 base freq root = ((map (\x -> (x,base))).(map (makeLength base))) (filter (\x -> (sum (conferSetToInt base x)) `mod` root == 0) (map (toBase (freq+1)) [0..rangeMax]))
    where rangeMax = (freq+1) ^ base - 1

makeLength :: Integer -> [Integer] -> [Integer]
makeLength len list = genericReplicate (len - listLength) 0 ++ list
    where listLength = genericLength list

genCands3 :: Integer -> Integer -> Integer -> [Number]
genCands3 base freq root = ((map (\x -> (x,base))).(map (makeLength base))) (filter (\x -> (sum (conferSetToInt base x)) `mod` root == 0) (map (toBase (freq+1)) [0..rangeMax]))
    where   rangeMax = (freq+1) ^ (base-1) -2
