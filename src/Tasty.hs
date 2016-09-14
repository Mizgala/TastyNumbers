module Tasty where
import Generation
import Numbers
import Misc
import Data.List (sort,nub,genericLength)

type Merge = ([Integer], Integer)

mergeProjection :: Projection -> Merge
mergeProjection (nums, base)    = (set, base)
    where set = foldl (++) [] (tail nums)

weight :: Integer -> [Integer] -> Integer
weight num = genericLength . filter (== num)

getWeights :: Number -> [Integer]
getWeights num = mergeLists (fst num) (getWeights' (snd num) (fst number))
        where number = (mergeProjection . projection) num

getWeights' :: Integer -> [Integer] -> [Integer]
getWeights' base nums = getWeights'' 0 base nums

getWeights'' :: Integer -> Integer -> [Integer] -> [Integer]
getWeights'' start base nums
    | start == base     = []
    | otherwise         = weight start nums : getWeights'' (start + 1) base nums

mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists [] []        = []
mergeLists nums []      = head nums : mergeLists (tail nums) []
mergeLists [] nums      = head nums : mergeLists [] (tail nums)
mergeLists list1 list2  = (head list1 + head list2) : mergeLists (tail list1) (tail list2)

isTasty :: Number -> Bool
isTasty num = uniqueWeights == 1
    where uniqueWeights = (length . nub . getWeights) num

isTasty2 :: Number -> Bool
isTasty2 num = (isTasty.fromPConferSet) num

findTasties :: Integer -> Integer -> [Number]
findTasties base weight = filter isTasty (genCands base weight)

baseTwoTasty :: Integer -> Number
baseTwoTasty ones = ([zeros, ones], 2)
    where   zeros = (snd weights) - (fst weights)
            weights = (makeTuple . getWeights) ([0,ones],2)

makeTuple :: [Integer] -> (Integer, Integer)
makeTuple lengthTwoList = (head lengthTwoList, (head . tail) lengthTwoList)

isAlmostTasty :: Number -> Bool
isAlmostTasty num = uniqueWeights == 1
    where uniqueWeights = (length . nub . tail . getWeights) num

makeTasty :: Number -> Number
makeTasty num = ((head digits) + newZeros : (tail digits), snd num)
    where   zeros = (head . getWeights) num
            ones = (head . tail . getWeights) num
            newZeros = ones - zeros
            digits = fst num

constructEvenBaseTasty :: Integer -> Number
constructEvenBaseTasty base = ([0] ++ (newReplicate group1 1) ++ [0] ++ [1,1,1,1] ++ [0] ++ (newReplicate group2 1), base)
    where   group1 = (base `div` 2 - 4)
            group2 = (base `div` 2 - 3)

constructWeightTwoTasty :: Integer -> Number
constructWeightTwoTasty base
    | base == 5 = ([1,2,0,2,2],5)
    | otherwise = ([1,2,1] ++ (newReplicate (base - 6) 2) ++ [1,2,2],base)

constructWeightFourTasty :: Integer -> Number
constructWeightFourTasty base = ([3,4,4,3] ++ (newReplicate (base - 7) 4) ++ [3,3,4],base)
