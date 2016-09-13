module Main where
import Tasty
import Data.List
import Numbers (toPConferSet)

main = do
        contents <- getLine
        putStr (getOut contents)
        putStr "\n"
        
getOut :: String -> String
getOut input = show(map toPConferSet out)
    where out = 

lineToList :: String -> [String]
lineToList input = words input

strListToIntegerTuple :: [String] -> (Integer,Integer)
strListToIntegerTuple (base:freq:[]) = (intBase, intFreq)
    where intBase = read base :: Integer
          intFreq = read freq :: Integer

getBase :: (Integer,Integer) -> Integer
getBase (base,freq) = base

getFreq :: (Integer,Integer) -> Integer
getFreq (base,freq) = freq
