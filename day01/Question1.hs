{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Data.Char (isDigit)

getFirstDigit :: String -> Char
getFirstDigit code = 
    case find isDigit code of
        Nothing -> error "No digit found in code"
        Just x -> x

getLastDigit :: String -> Char
getLastDigit = getFirstDigit . reverse

apply2 :: (a -> b) -> (a -> c) -> a -> (b, c)
apply2 f g x = (f x, g x)

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

main :: IO ()
main = do
    codes <- lines <$> (readFile "day01/input.txt")
    let answer = sum 
                $ (flip map) codes 
                $ (read :: String -> Int) 
                . pairToList 
                . (apply2 getFirstDigit getLastDigit)
    print answer