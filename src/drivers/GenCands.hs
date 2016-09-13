module Main where
import Tasty
import Data.List
import Generation
import Numbers (toPConferSet, conferSetToInt)

main = do
        contents <- getLine
        putStr (getOut contents)
        putStr "\n"
        
getOut :: String -> String
getOut input = show(filter (\x -> (sum.(conferSetToInt(snd x))) (fst x) `mod` root == 0) out)
    where   out = genCands base freq
            base = (getBase.strListToIntegerTuple.lineToList) input
            freq = (getFreq.strListToIntegerTuple.lineToList) input
            root = (getRoot.strListToIntegerTuple.lineToList) input
    

lineToList :: String -> [String]
lineToList input = words input

strListToIntegerTuple :: [String] -> (Integer,Integer,Integer)
strListToIntegerTuple (base:freq:root:[]) = (intBase, intFreq, intRoot)
    where intBase = read base :: Integer
          intFreq = read freq :: Integer
          intRoot = read root :: Integer

getBase :: (Integer,Integer,Integer) -> Integer
getBase (base,freq,root) = base

getFreq :: (Integer,Integer,Integer) -> Integer
getFreq (base,freq,root) = freq

getRoot :: (Integer,Integer,Integer) -> Integer
getRoot (base,freq,root) = root
