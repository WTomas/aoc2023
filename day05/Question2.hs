{-# LANGUAGE MultiWayIf #-}

import qualified Data.Text as T
import qualified Data.List as L
import Control.Arrow

readInt :: String -> Int
readInt = read

data MapRange = MapRange {
      sourceRange :: Range
    , destinationRange :: Range
} deriving (Show)

data Map = Map {
      source :: String
    , destination :: String
    , ranges :: [MapRange]
} deriving (Show)

type Range = (Int, Int)

getSourceName :: String -> String
getSourceName s = takeWhile (/= '-') s

getDestinationName :: String -> String
getDestinationName s = (reverse . (takeWhile (/= '-')) . reverse . (takeWhile (/= ' '))) s

getMapRange :: [Int] -> MapRange
getMapRange [destinationStartRange', sourceStartRange', rangeLength'] = MapRange (sourceStartRange', sourceStartRange' + rangeLength' - 1) (destinationStartRange', destinationStartRange' + rangeLength' - 1)

getMap :: ((String, String), [MapRange]) -> Map
getMap ((source', destination'), ranges') = Map source' destination' ranges'

transferRange :: Range -> Range -> Range -> Range
transferRange (l, u) (l', u') (dl, du) = (l-l'+dl, u-l'+dl)

intersects :: Range -> Range -> Bool
intersects (l, u) (l', u') = not (u < l' || u' < l )

getDestinationRange :: Range -> MapRange -> (Range, [Range])
getDestinationRange currentRange (MapRange { sourceRange, destinationRange }) = do
  let (l, u) = currentRange
  let (l', u') = sourceRange
  let (dl, du) = destinationRange
  if | l' <= l && u <= u' -> (transferRange currentRange sourceRange destinationRange, [])
     | l' <= l && u'<= u -> (transferRange (l, u') sourceRange destinationRange, [(u'+1, u)])
     | otherwise -> (transferRange (l', u) sourceRange destinationRange, [(l, l'-1)])

getDestinationRanges :: [Range] -> [MapRange] -> [Range]
getDestinationRanges ranges mapRanges = getDestinationRanges' ranges [] mapRanges
  where
    getDestinationRanges' :: [Range] -> [Range] -> [MapRange] -> [Range]
    getDestinationRanges' [] destinationRanges _ = destinationRanges
    getDestinationRanges' (currentRange:remainingRanges) destinationRanges mapRanges = do
      let maybeMapRange = L.find (\(MapRange {sourceRange, destinationRange}) -> currentRange `intersects` sourceRange) mapRanges
      case maybeMapRange of 
        Nothing -> getDestinationRanges' remainingRanges (currentRange:destinationRanges) mapRanges
        Just mapRange -> do
          let (destinationRange, newRemainingRanges) = getDestinationRange currentRange mapRange
          getDestinationRanges' (remainingRanges++newRemainingRanges) (destinationRange:destinationRanges) mapRanges

transformSeedRanges :: [Int] -> [Range]
transformSeedRanges []  = []
transformSeedRanges [_] = []
transformSeedRanges (start':length':rest) = (start', start'+length'-1):(transformSeedRanges rest)

main :: IO()
main = do
    file <- T.pack <$> readFile "day05/input.txt"
    let groups = T.splitOn (T.pack "\n\n") file
    let seedRanges = (transformSeedRanges . (map readInt) . (map T.unpack) . tail . T.words . head) groups
    let maps = map (
            getMap
          . (((getSourceName &&& getDestinationName) . head) &&& (map (getMapRange . (map readInt) . words)) .  tail)
          . map T.unpack
          . (T.splitOn (T.pack "\n"))
          ) (tail groups)

    print $ (minimum . map fst . foldl getDestinationRanges seedRanges) (map ranges maps)
