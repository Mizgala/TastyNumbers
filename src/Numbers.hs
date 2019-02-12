{-# OPTIONS_GHC -Wall #-}
module Numbers where

--  These are the two types that are used as the basis for
--  represnetation in all files derived from this one. Numbers are
--  represented as a tuple of their Confer Set Notation (CSN) and
--  their base. Projections are tuples of a list of lists and the
--  base. The head of the list is the CSN and the tail of the list
--  is every step of the projection.
type Number = ([Integer], Integer)
type Projection = ([[Integer]], Integer)

--  This function takes a base and a list of numbers (usually assumed
--  to be a CSN) and returns the two in Number form.
makeNumber :: Integer -> [Integer] -> Number
makeNumber base set = (set, base)

--  This is the primary function to convert between bases and serves
--  as the first filter to the process. This functions takes a base
--  and a base 10 integer and does one of two things. If the number
--  is 0, then the function returns [0], as 0 is the same in every
--  base. If it is not 0, then the function returns the result of 
--  toBase'.
toBase :: Integer -> Integer -> [Integer]
toBase base num
    | num == 0      = [0]
    | otherwise     = toBase' base num

--  This is the second filter function of the toBase process. If a
--  base is negative, then this function returns the result of
--  toBaseN. If the base is positive, it returns toBaseP. If the base
--  is neither, something has gone wrong and the function returns an
--  empty list. A negative number in a positive base is represented
--  normaly but with all members of the list multiplied by -1. While
--  this is not the most asthetically pleasing method, it works for
--  other functions.
toBase' :: Integer -> Integer -> [Integer]
toBase' base num
    | base > 0  = if (num > 0) then toBaseP base num else ((map (*(-1))). (toBaseP base)) (abs num)
    | base < 0  = (reverse.(map abs) . (toBaseN base)) num
    | otherwise = []

-- This is one of the two final steps for base conversion. This
-- functions assumes that it is being passed a non-zero number and
-- that the user wants to get back the number in a positive base.
toBaseP :: Integer -> Integer -> [Integer]
toBaseP base num
    | num == 0  = []
    | otherwise = (toBaseP base quotient) ++ [remainder]
    where   remainder   = num `mod` base
            quotient    = num `div` base

--  This is the counterpart to toBaseP. Given that the algorithm for
--  converting to negative bases requires a bit more intuition, this
--  function serves as another filter that works based on whether or
--  no num is positive.
toBaseN :: Integer -> Integer -> [Integer]
toBaseN base num
    | num > 0   = toBaseNP base num
    | num < 0   = toBaseNN base num
    | otherwise = []

--  This function is the calculation function for when num is
--  positive. This function calls back to toBaseN when it is done.
toBaseNP :: Integer -> Integer -> [Integer]
toBaseNP base num = remainder : (toBaseN base quotient)
    where   remainder   = num `mod` (abs base)
            quotient    = if (remainder == 0) then num `div` base else (num `div` base) + 1

--  This function serves the same purpose as toBaseNP but for negative
--  values of num. There are seemingly many redundencies in this
--  function but they are neccesary to accomadate rounding by the
--  computer.
toBaseNN :: Integer -> Integer -> [Integer]
toBaseNN base num = remainder : (toBaseN base quotient)
    where   quotient    = abs (num `div` (abs base))
            remainder   = (quotient * base) - num

--  This is the collection of functions that calculates the Projection
--  of a Number.
--  TODO: clean up these functions
projection2 :: Number -> Projection
projection2 num = ((fst num : takeNums (projection' (projection'' num))), snd num)

projection' :: Number -> [Number]
projection' (num, base)
    | (length num) == 1 = [(num, base)]
    | otherwise         = (num, base) : projection' ((toBase base (sum num)), base)

projection'' :: Number -> Number
projection'' num = (conferSetToInt (snd num) (fst num), snd num)

projection :: Number -> Projection
projection num = ((reverse.head.fst) proj : (tail.fst) proj, snd proj)
    where proj = (projection2.fromPConferSet) num

--  This function takes a list of Numbers and returns a list of their
--  CSN's (the first member of each tuple).
takeNums :: [Number] -> [[Integer]]
takeNums []         = []
takeNums [num]      = [fst num]
takeNums (num:nums) = (fst num) : takeNums nums

--  The following functions are used to convert between CSN's and
--  Integers or the decomposition of Integers.

--  conferSetToInt takes a base and the reverse CSN and returns the
--  decomposition using the function conferSetToInt' and the toBase
--  function.
conferSetToInt :: Integer -> [Integer] -> [Integer]
conferSetToInt base nums = toBase base (conferSetToInt' 0 nums)

--  This function takes a starting number and the reverse CSN and
--  converts it to the base 10 equivalent.
conferSetToInt' :: Integer -> [Integer] -> Integer
conferSetToInt' start nums
    | nums == []    = 0
    | otherwise     = (head nums) * start + conferSetToInt' (start+1) (tail nums)

--  This function converts a Number from its reverse CSN to its proper
--  CSN.
toPConferSet :: Number -> Number
toPConferSet (nums, base) = (reverse nums, base)

--  This is the same function as toPConferSet but under a different
--  name. This function is intended to reverse toPConferSet.
fromPConferSet :: Number -> Number
fromPConferSet = toPConferSet

--  This function takes a Number and calculates the digit root of
--  that Number.
getRoot :: Number -> Integer
getRoot = head . head . reverse . fst . projection

--  This function takes a Number and converts it to a base 10 Integer.
toBase10 :: Number -> Integer
toBase10 num = (sum.(toBase10' (snd num) 0)) ((reverse.(conferSetToInt (snd num)).fst) num)

toBase10' :: Integer -> Integer -> [Integer] -> [Integer]
toBase10' _ _ []            = []
toBase10' base pow [x]      = [(base ^ pow) * x]
toBase10' base pow (x:xs)   = ((base ^ pow) * x) : toBase10' base (pow + 1) xs
