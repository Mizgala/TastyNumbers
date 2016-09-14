module Main where
import Tasty
import Data.List
import Generation
import Numbers
import System.Environment

main = do
        contents <- getArgs
        putStr "Finding candidates...\n"
        putStr ((show.genericLength.getCands) contents)
        putStr " candidates found.\n"
        putStr "Searching candidates for tasties...\n"
        putStr (show (map toPConferSet (filter isTasty (getCands contents))))
        putStr "\n"
        
getCands :: [String] -> [Number]
getCands input = filter (\x -> (sum.(conferSetToInt(snd x))) (fst x) `mod` root == 0) out
    where   out = genCands2 base weight root
            base = (getBase.strListToIntegerTuple) input
            weight = (getWeight.strListToIntegerTuple) input
            root = (getIntRoot.strListToIntegerTuple) input
    

lineToList :: String -> [String]
lineToList input = words input

strListToIntegerTuple :: [String] -> (Integer,Integer,Integer)
strListToIntegerTuple (base:weight:root:[]) = (intBase, intWeight, intRoot)
    where intBase = read base :: Integer
          intWeight = read weight :: Integer
          intRoot = read root :: Integer

getBase :: (Integer,Integer,Integer) -> Integer
getBase (base,weight,root) = base

getWeight :: (Integer,Integer,Integer) -> Integer
getWeight (base,weight,root) = weight

getIntRoot :: (Integer,Integer,Integer) -> Integer
getIntRoot (base,weight,root) = root
