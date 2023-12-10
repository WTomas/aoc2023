import qualified Data.Text as T
import qualified Data.List as L
import Control.Arrow

readInt :: String -> Int
readInt = read

data MapRange = MapRange {
      sourceStartRange :: Int
    , destinationStartRange :: Int
    , rangeLength :: Int
} deriving (Show)

data Map = Map {
      source :: String
    , destination :: String
    , ranges :: [MapRange]
} deriving (Show)

getSourceName :: String -> String
getSourceName s = takeWhile (/= '-') s

getDestinationName :: String -> String
getDestinationName s = (reverse . (takeWhile (/= '-')) . reverse . (takeWhile (/= ' '))) s

getMapRange :: [Int] -> MapRange
getMapRange [destinationStartRange', sourceStartRange', rangeLength'] = MapRange sourceStartRange' destinationStartRange' rangeLength'

getMap :: ((String, String), [MapRange]) -> Map
getMap ((source', destination'), ranges') = Map source' destination' ranges'

getDestinationValue :: Int -> Map -> Int
getDestinationValue sourceValue map' = do
    let maybeRange = L.find (\range -> (sourceStartRange range) <= sourceValue && (sourceStartRange range + rangeLength range) > sourceValue) (ranges map')
    case maybeRange of 
        Nothing -> sourceValue
        Just range -> sourceValue - (sourceStartRange range) + (destinationStartRange range)

getDestinationValues :: [Int] -> Map -> [Int] 
getDestinationValues sourceValues map' = map ((flip getDestinationValue) map') sourceValues

main :: IO()
main = do
    file <- T.pack <$> readFile "day05/input.txt"
    let groups = T.splitOn (T.pack "\n\n") file
    let seeds = ((map readInt) . (map T.unpack) . tail . T.words . head) groups
    let maps = map (
            getMap
          . (((getSourceName &&& getDestinationName) . head) &&& (map (getMapRange . (map readInt) . words)) .  tail)
          . map T.unpack
          . (T.splitOn (T.pack "\n"))
          ) (tail groups)

    print $ minimum $ (foldl getDestinationValues seeds) maps
