module Hex where

import Data.Char
import Data.Array

valueOfChar :: Char -> Int
valueOfChar c = do
    case (toLower c) of
        'a' -> 10
        'b' -> 11
        'c' -> 12
        'd' -> 13
        'e' -> 14
        'f' -> 15
        _ -> 0

valueOfInt :: Char -> Int
valueOfInt c = do
    if (isDigit c)
        then (digitToInt c)
        else (valueOfChar c)

-- use to run string
runHexString :: String -> Int -> Int -> Int
runHexString str total i = do
    if (i < length str)
        then do
            let value = (valueOfInt (str!!i)) * (16^((length str) - (i + 1)))
            runHexString str (total + value) (i + 1)
        else total
        
-- simplified "runHexString"
decode :: String -> Int
decode str = do
        runHexString str 0 0
