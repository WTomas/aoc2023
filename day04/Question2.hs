{-# LANGUAGE ScopedTypeVariables #-}

import Control.Arrow
import qualified Data.Set as S

readInt :: String -> Int
readInt = read

getMatchingNumbers :: [Int] -> [Int] -> [Int]
getMatchingNumbers winningNumbers chosenNumbers = let winningNumberSet = S.fromList winningNumbers in 
    filter ((flip S.member) winningNumberSet) chosenNumbers

takeBetweenIndices :: Int -> Int -> [a] -> [a]
takeBetweenIndices from to xs = snd . unzip . (filter (\(idx, _) -> (idx >= from && idx < to))) $ zip [0..] xs

updateCardCount :: [Int] -> (Int, Int) -> [Int]
updateCardCount oldCounts (index, nWinning) = let nextIndex = index + 1 in
    (takeBetweenIndices 0 nextIndex oldCounts)
    ++ ((map (+ oldCounts !! index)) . (takeBetweenIndices nextIndex (nextIndex + nWinning))) oldCounts
    ++ takeBetweenIndices (nextIndex + nWinning) (length oldCounts) oldCounts


main :: IO ()
main = do
    games <- lines <$> readFile "day04/input.txt"
    print $ sum
          . foldl updateCardCount (take (length games) $ repeat 1)
          . (zip [0..])
          . map (
              length
            . uncurry getMatchingNumbers 
            . (((map readInt) . fst) &&& ((map readInt) . snd))
            . ((words . fst) &&& (words . snd))
            . (fst &&& (tail . snd))
            . span (/= '|')
            . tail
            . dropWhile (/= ':')
            ) $ games
