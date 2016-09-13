module Main where
import Tasty
import Data.List
import Generation
import Numbers
import System.Environment

main = do
        contents <- getArgs
        putStr (show (getCands contents))
        putStr "\n"
        
getCands :: [String] -> Number
getCands input = out
    where   out = constructEvenBaseTasty1 base
            base = (strToInteger) input

strToInteger :: [String] -> Integer
strToInteger (base:[]) = intBase
    where intBase = read base :: Integer
