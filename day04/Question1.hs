{-# LANGUAGE ScopedTypeVariables #-}

import Control.Arrow
import qualified Data.Set as S

readInt :: String -> Int
readInt = read

getMatchingNumbers :: [Int] -> [Int] -> [Int]
getMatchingNumbers winningNumbers chosenNumbers = let winningNumberSet = S.fromList winningNumbers in 
    filter ((flip S.member) winningNumberSet) chosenNumbers

getScore :: [Int] -> Int
getScore [] = 0
getScore matchingNumbers = 2 ^ (length matchingNumbers - 1)

main :: IO ()
main = do
    games <- lines <$> readFile "day04/input.txt"
    print $ sum . map (
          getScore 
        . uncurry getMatchingNumbers 
        . (((map readInt) . fst) &&& ((map readInt) . snd))
        . ((words . fst) &&& (words . snd))
        . (fst &&& (tail . snd))
        . span (/= '|')
        . tail
        . dropWhile (/= ':')
        ) $ games