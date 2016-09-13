module Tasty where
import Generation
import Numbers
import Misc
import Data.List (sort,nub,genericLength)

type Merge = ([Integer], Integer)

mergeProjection :: Projection -> Merge
mergeProjection (nums, base)    = (set, base)
    where set = foldl (++) [] (tail nums)

frequency :: Integer -> [Integer] -> Integer
frequency num = genericLength . filter (== num)

getFrequencies :: Number -> [Integer]
getFrequencies num = mergeLists (fst num) (getFrequencies' (snd num) (fst number))
        where number = (mergeProjection . projection) num

getFrequencies' :: Integer -> [Integer] -> [Integer]
getFrequencies' base nums = getFrequencies'' 0 base nums

getFrequencies'' :: Integer -> Integer -> [Integer] -> [Integer]
getFrequencies'' start base nums
    | start == base     = []
    | otherwise         = frequency start nums : getFrequencies'' (start + 1) base nums

mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists [] []        = []
mergeLists nums []      = head nums : mergeLists (tail nums) []
mergeLists [] nums      = head nums : mergeLists [] (tail nums)
mergeLists list1 list2  = (head list1 + head list2) : mergeLists (tail list1) (tail list2)

isTasty :: Number -> Bool
isTasty num = uniqueFrequencies == 1
    where uniqueFrequencies = (length . nub . getFrequencies) num

isTasty2 :: Number -> Bool
isTasty2 num = (isTasty.fromPConferSet) num

findTasties :: Integer -> Integer -> [Number]
findTasties base freq = filter isTasty (genCands base freq)

baseTwoTasty :: Integer -> Number
baseTwoTasty ones = ([zeros, ones], 2)
    where   zeros = (snd freqs) - (fst freqs)
            freqs = (makeTuple . getFrequencies) ([0,ones],2)

makeTuple :: [Integer] -> (Integer, Integer)
makeTuple lengthTwoList = (head lengthTwoList, (head . tail) lengthTwoList)

isAlmostTasty :: Number -> Bool
isAlmostTasty num = uniqueFrequencies == 1
    where uniqueFrequencies = (length . nub . tail . getFrequencies) num

makeTasty :: Number -> Number
makeTasty num = ((head digits) + newZeros : (tail digits), snd num)
    where   zeros = (head . getFrequencies) num
            ones = (head . tail . getFrequencies) num
            newZeros = ones - zeros
            digits = fst num

constructEvenBaseTasty :: Integer -> Number
constructEvenBaseTasty base = ([0] ++ (newReplicate group1 1) ++ [0] ++ [1,1,1,1] ++ [0] ++ (newReplicate group2 1), base)
    where   group1 = (base `div` 2 - 4)
            group2 = (base `div` 2 - 3)

constructFreqTwoTasty :: Integer -> Number
constructFreqTwoTasty base
    | base == 5 = ([1,2,0,2,2],5)
    | otherwise = ([1,2,1] ++ (newReplicate (base - 6) 2) ++ [1,2,2],base)
