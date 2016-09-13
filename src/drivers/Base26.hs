module Main where
import Tasty
import Data.List
import Generation
import Numbers
import System.Environment

main = do
        contents <- getArgs
        putStr "Finding candidates...\n"
        putStr ((show.getCands) contents)
        putStr "\n"
        putStr "Searching candidates for tasties...\n"
        putStr (show (map toPConferSet (filter isTasty (getCands contents))))
        putStr "\n"
        
getCands :: [String] -> [Number]
getCands input = map (\x -> ((toBase 26 x), 26)) (map (*25) [0..(237516935566761825617270029763814725 `div` 25)])
    where   out = genCands base freq
            base = (getBase.strListToIntegerTuple) input
            freq = (getFreq.strListToIntegerTuple) input
            root = (getRoot.strListToIntegerTuple) input
    

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
