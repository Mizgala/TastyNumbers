module Tasty where
import Generation
import Numbers
import Misc
import Data.List (sort,nub,genericLength)

--  The Merge type is a close variant of the Projection type. The
--  difference is that, rather than having a list of lists to contain
--  the projection, a Merge simply has all elements of a Projection's
--  lists in a single list.
type Merge = ([Integer], Integer)

--  This function converts a Projection to a merge.
mergeProjection :: Projection -> Merge
mergeProjection (nums, base)    = (set, base)
    where set = foldl (++) [] (tail nums)

--  This function counts the number of times that an Integer appears
--  in a list.
weight :: Integer -> [Integer] -> Integer
weight num = genericLength . filter (== num)

--  The following functions are used to get the weights of all digits
--  in a base while looking at a specific Number. This function is
--  primarily used in the checking whether or not a Number is Tasty.
getWeights :: Number -> [Integer]
getWeights num = mergeLists (fst num) (getWeights' ((abs.snd) num) (fst number))
        where number = (mergeProjection . projection) num

getWeights' :: Integer -> [Integer] -> [Integer]
getWeights' base nums = getWeights'' 0 base nums

getWeights'' :: Integer -> Integer -> [Integer] -> [Integer]
getWeights'' start base nums
    | start == base     = []
    | otherwise         = weight start nums : getWeights'' (start + 1) base nums

-- If this returns -1, then num is not a tasty.
order :: Number -> Integer
order num = countTasties nums - 1
    where   countTasties = genericLength . (filter (isTasty))
            nums = (projectionToNumbers'.projection2) num

projectionToNumbers :: Projection -> [Number]
projectionToNumbers proj
    | (snd proj) > 0    = projectionToNumbers' proj
    | (snd proj) < 0    = ((map negateBase).projectionToNumbers') (fst proj, (abs.snd) proj)
    | otherwise         = []
    where   negateBase = \x -> (fst x, (snd x) * (-1))

projectionToNumbers' :: Projection -> [Number]
projectionToNumbers' proj = ((head.fst) proj, snd proj) : projectionToNumbers'' ((tail.fst) proj, snd proj)

projectionToNumbers'' :: Projection -> [Number]
projectionToNumbers'' ([],_)  = []
projectionToNumbers'' proj = (projectionToNumbers''' (snd proj - 1) ((head.fst) proj),snd proj) : projectionToNumbers'' ((tail.fst) proj, snd proj)

projectionToNumbers''' :: Integer -> [Integer] -> [Integer]
projectionToNumbers''' start num
    | start == 0    = [weight 0 num]
    | otherwise     = weight start num : projectionToNumbers''' (start - 1) num

mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists [] []        = []
mergeLists nums []      = head nums : mergeLists (tail nums) []
mergeLists [] nums      = head nums : mergeLists [] (tail nums)
mergeLists list1 list2  = (head list1 + head list2) : mergeLists (tail list1) (tail list2)

isTasty :: Number -> Bool
isTasty num = ((not.elementsRepeat.projection) num) && (uniqueWeights == 1)
    where   uniqueWeights = (genericLength.nub.fst.sums) nums
            sums = foldl (numberFunction (+)) (newReplicate ((abs.snd) num) 0, ((snd) num))
            proj = projection num
            nums = projectionToNumbers proj

elementsRepeat :: Projection -> Bool
elementsRepeat proj = elementsRepeat' (fst proj)

elementsRepeat' :: [[Integer]] -> Bool
elementsRepeat' []          = False
elementsRepeat' [x]         = False
elementsRepeat' [x1,x2]     = x1 == x2
elementsRepeat' (x1:x2:xs)  = if x1 == x2 then x1 == x2 else elementsRepeat' (x2:xs)

findTasties :: Integer -> Integer -> [Number]
findTasties base weight = if base < 0 then ((filter isTasty).(map negateBase)) (genCands (abs base) weight) else filter isTasty (genCands base weight)
    where   negateBase = \x -> (fst x, (snd x) * (-1))

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

constructTasty :: Integer -> Integer -> Number
constructTasty weight' base = func weight' base
    where func = if even weight' then constructEvenWeightTasty else constructOddWeightTasty

constructWeightOneTasty :: Integer -> Number
constructWeightOneTasty base
    | even base = constructWeightOneTastyE base
    | otherwise = constructWeightOneTastyO base

constructWeightOneTastyO :: Integer -> Number
constructWeightOneTastyO base = ((newReplicate group1 1) ++ [0,0] ++ (newReplicate group2 1) ++ [0,1],base)
    where   group1 = (base + 1) `div` 2 - 1
            group2 = group1 - 3

constructWeightOneTastyE :: Integer -> Number
constructWeightOneTastyE base = ([0] ++ (newReplicate group1 1) ++ [0] ++ [1,1,1,1] ++ [0] ++ (newReplicate group2 1), base)
    where   group1 = (base `div` 2 - 4)
            group2 = (base `div` 2 - 3)

constructWeightTwoTasty :: Integer -> Number
constructWeightTwoTasty base
    | base == 5 = ([1,2,0,2,2],5)
    | otherwise = ([1,2,1] ++ (newReplicate (base - 6) 2) ++ [1,2,2],base)

constructWeightFourTasty :: Integer -> Number
constructWeightFourTasty base = ([3,4,4,3] ++ (newReplicate (base - 7) 4) ++ [3,3,4],base)

constructWeightThreeTastyO :: Integer -> Number
constructWeightThreeTastyO base = ((newReplicate ((base+1) `div` 2 - 1) 3 ) ++ [2,3,2] ++ ((newReplicate ((base+1) `div` 2 - 5) 3) ++ [1,3]), base)

constructWeightThreeTastyE :: Integer -> Number
constructWeightThreeTastyE base = ([2] ++ (newReplicate ((base `div` 2) - 4) 3) ++ [2,3,3,3,3,3,2] ++ (newReplicate ((base `div` 2) - 6) 3) ++ [2,3], base)

constructWeightThreeTasty :: Integer -> Number
constructWeightThreeTasty base
    | even base = constructWeightThreeTastyE base
    | otherwise = constructWeightThreeTastyO base

constructWeightSixTasty :: Integer -> Number
constructWeightSixTasty base = ([5,6,6,6,5] ++ (newReplicate (base - 8) 6) ++ [4,6,6],base)

constructEvenWeightTasty :: Integer -> Integer -> Number
constructEvenWeightTasty weight' base
    | odd weight'                       = ([],base)
    | base < ((weight' `div` 2) + 5)    = ([],base)
    | weight' == 2                      = constructWeightTwoTasty base
    | otherwise                         = constructEvenWeightTasty' weight' base

constructOddWeightTasty :: Integer -> Integer -> Number
constructOddWeightTasty weight' base
    | even weight'                      = ([],base)
    | weight' == 1                      = constructWeightOneTasty base
    | otherwise                         = constructNewTasty weight' base
    where   constructNewTasty = if even base then constructOddWeightTastyE else constructOddWeightTastyO

constructEvenWeightTasty' :: Integer -> Integer -> Number
constructEvenWeightTasty' weight' base = numberFunction (-) (newReplicate base weight',base) inverseNumber
    where inverseNumber = makeInverseE weight' base

makeInverseE :: Integer -> Integer -> Number
makeInverseE weight' base = (makeList (base-1) [n1,n2,n3,n4],base) 
    where   n1 = base - 1
            n2 = base - ((weight' `div` 2) +2)
            n3 = 2
            n4 = 1 + ((weight' `div` 2) -2)

makeInverseOE :: Integer -> Integer -> Number
makeInverseOE weight' base = (makeList (base-1) [n1,n2,n3,n4],base)
    where   n1 = base - 1
            n2 = (base `div` 2) + 2
            n3 = (base `div` 2) - ((weight' + 1) `div` 2 + 2)
            n4 = (weight' - 1) `div` 2

makeInverseOO :: Integer -> Integer -> Number
makeInverseOO weight' base = (makeList (base-1) [n1,n2,n3,n4],base)
    where   n1 = (base - 1) `div` 2
            n2 = 1
            n3 = (weight' - 1) `div` 2
            n4 = ((base - 1) `div` 2) - ((weight' + 1) `div` 2)

makeList :: Integer -> [Integer] -> [Integer]
makeList start ns
    | start == 0    = [weight 0 ns]
    | otherwise     = weight start ns : makeList (start - 1) ns

constructOddWeightTastyE :: Integer -> Integer -> Number
constructOddWeightTastyE weight' base = numberFunction (-) (newReplicate base weight', base) inverseNumber
    where inverseNumber = makeInverseOE weight' base

constructOddWeightTastyO :: Integer -> Integer -> Number
constructOddWeightTastyO weight' base = numberFunction (-) (newReplicate base weight', base) inverseNumber
    where inverseNumber = makeInverseOO weight' base
