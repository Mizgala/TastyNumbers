{-# OPTIONS_GHC -Wall #-}
module Generation where
import Data.List(genericLength, genericReplicate)
import Numbers
import Misc

{-  This is a set of functions used to generate candidates to look for Tasty
 -  numbers.-}

maxCandidate :: Integer -> Integer -> [Integer]
maxCandidate base weight = map (* weight) (newReplicate base 1) 

candRange :: Integer -> Integer -> [[Integer]]
candRange base weight = map (range 0) (maxCandidate base weight)

range :: Integer -> Integer -> [Integer]
range start end = [start..end]

combos :: [[a]] -> [[a]]
combos []   = [[]]
combos ([]:ls)  = combos ls
combos ((h:t):ls) = map (h:) (combos ls) ++ combos (t:ls)

lengthCheck :: Integer -> [Integer] -> Bool
lengthCheck base list = base == (genericLength list)

genCands' :: Integer -> Integer -> [[Integer]]
genCands' base weight = (filter (lengthCheck base) . combos . (candRange base)) weight

genCands :: Integer -> Integer -> [Number]
genCands base weight = map (makeNumber base) (genCands' base weight)

makeSet :: Integer -> [Integer] -> [Integer]
makeSet _ [] = []
makeSet start (num:[]) = newReplicate num start
makeSet start (num:nums) = (newReplicate num start) ++ (makeSet (start + 1) nums)

genCands2 :: Integer -> Integer -> Integer -> [Number]
genCands2 base weight root = ((map (\x -> (x,base))).(map (makeLength base))) (filter (\x -> (sum (conferSetToInt base x)) `mod` root == 0) (map (toBase (weight+1)) [0..rangeMax]))
    where rangeMax = (weight+1) ^ base - 1

makeLength :: Integer -> [Integer] -> [Integer]
makeLength len list = genericReplicate (len - listLength) 0 ++ list
    where listLength = genericLength list

genCands3 :: Integer -> Integer -> Integer -> [Number]
genCands3 base weight root = ((map (\x -> (x,base))).(map (makeLength base))) (filter (\x -> (sum (conferSetToInt base x)) `mod` root == 0) (map (toBase (weight+1)) [0..rangeMax]))
    where   rangeMax = (weight+1) ^ (base-1) -2
