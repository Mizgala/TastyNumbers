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
getCands [weightS,baseS] = out
    where   out = constructTasty w base
            base = (strToInteger) baseS
            w = (strToInteger) weightS

strToInteger :: String -> Integer
strToInteger (base) = intBase
    where intBase = read base :: Integer
