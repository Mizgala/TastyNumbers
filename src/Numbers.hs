{-# OPTIONS_GHC -Wall #-}
module Numbers where

type Number = ([Integer], Integer)
type Projection = ([[Integer]], Integer)

makeNumber :: Integer -> [Integer] -> Number
makeNumber base set = (set, base)

toBase :: Integer -> Integer -> [Integer]
toBase base num
    | (toBase' base) num == [0] = [0]
    | otherwise                 = (tail . reverse . (toBase' base)) num

toBase' :: Integer -> Integer -> [Integer]
toBase' base num
    | num == 0  = [0]
    | otherwise = (num `mod` base) : (toBase' base (num `div` base))

projection :: Number -> Projection
projection num = ((fst num : takeNums (projection' (projection'' num))), snd num)

projection' :: Number -> [Number]
projection' (num, base)
    | (length num) == 1 = [(num, base)]
    | otherwise         = (num, base) : projection' ((toBase base (sum num)), base)

projection'' :: Number -> Number
projection'' num = (conferSetToInt (snd num) (fst num), snd num)

projection2 :: Number -> Projection
projection2 num = ((reverse.head.fst) proj : (tail.fst) proj, snd proj)
    where proj = (projection.fromPConferSet) num

takeNums :: [Number] -> [[Integer]]
takeNums []         = []
takeNums [num]      = [fst num]
takeNums (num:nums) = (fst num) : takeNums nums

conferSetToInt :: Integer -> [Integer] -> [Integer]
conferSetToInt base nums = toBase base (conferSetToInt' 0 nums)

conferSetToInt' :: Integer -> [Integer] -> Integer
conferSetToInt' start nums
    | nums == []    = 0
    | otherwise     = (head nums) * start + conferSetToInt' (start+1) (tail nums)

toPConferSet :: Number -> Number
toPConferSet (nums, base) = (reverse nums, base)

fromPConferSet :: Number -> Number
fromPConferSet = toPConferSet

getRoot :: Number -> Integer
getRoot = head . head . reverse . fst . projection
