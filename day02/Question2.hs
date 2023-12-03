{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Data.Char
import Data.List
import Data.Foldable (maximumBy, Foldable)
import Data.Ord      (comparing)

maxBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maxBy = maximumBy . comparing

newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

data GameColour = Red | Green | Blue deriving (Show, Eq)
data Draw = Draw {
       count :: Int
    ,  colour :: GameColour
} deriving (Show)
type Round = [Draw]
 
parseCharPredicate :: (Char -> Bool) -> Parser Char
parseCharPredicate g = Parser f
    where 
        f :: String -> Maybe (Char, String)
        f [] = Nothing
        f (x:xs) 
            | g x = Just (x, xs)
            | otherwise = Nothing
            
parseChar :: Char -> Parser Char
parseChar c = parseCharPredicate $ \c' -> c == c'

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
                (c, input') <- p input
                Just (f c, input')

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input) 
    (<*>) (Parser f) (Parser a) = Parser $ \input -> do
        (f' , input') <- f input
        (a', input'') <- a input'
        Just (f' a', input'')

instance Alternative Parser where
    empty = Parser $ \input -> Nothing
    (<|>) (Parser a) (Parser b) = Parser $ \input -> do
        case a input of
            Just x -> Just x
            Nothing -> do
                case b input of
                    Just y -> Just y
                    Nothing -> Nothing

parseString :: String -> Parser String
parseString = sequenceA . map parseChar

parseSpan :: (Char -> Bool) -> Parser String
parseSpan predicate = Parser $ f ""
    where
        f :: String -> String -> Maybe (String, String)
        f previous [] = 
            if previous == "" 
            then Nothing 
            else Just (reverse previous, [])
        f previous input@(c:input') = 
            if predicate c 
            then f (c:previous) input' 
            else if previous == "" then Nothing else Just (reverse previous, input)

digitParser :: Parser Int
digitParser = (read :: String -> Int) <$> parseSpan isDigit

spaceParser :: Parser Char
spaceParser = parseChar ' '

gameParser :: Parser Int
gameParser = parseString "Game" *> spaceParser *> digitParser <* parseChar ':' <* spaceParser

gameColourParser :: Parser GameColour
gameColourParser = ((\_ -> Green) <$> parseString "green") 
               <|> ((\_ -> Red) <$> parseString "red") 
               <|> ((\_ -> Blue) <$> parseString "blue") 

perhaps :: Parser a -> Parser (Maybe a)
perhaps (Parser p) = Parser $ \input -> 
        case p input of
            Nothing -> Just (Nothing, input)
            Just (x, input') -> Just (Just x, input')

drawParser :: Parser Draw
drawParser = Draw <$> (digitParser <* spaceParser) <*> (gameColourParser <* perhaps ((parseChar ',') <* spaceParser))

roundParser :: Parser Round
roundParser = some drawParser <* perhaps (parseChar ';' <* spaceParser)

getMaximalDraws :: [Round] -> [Draw]
getMaximalDraws rounds = do
          (map (\roundGroup -> maxBy (\round -> count round) roundGroup)) 
        . (groupBy (\round1 round2 -> (colour round1) == (colour round2))) 
        . ((sortBy (\round1 round2 -> compare ((show . colour) round1) ((show . colour) round2)))) 
        . concat $  rounds

main :: IO()
main = do
    games <- lines <$> readFile "day02/input.txt"
    print $ (sum . (map f)) games
    where 
        f :: String -> Int
        f game = let parsedGames = (runParser ((,) <$> gameParser <*> some roundParser) game) in
            case parsedGames of 
                Nothing -> 0
                Just ((gameId, rounds), _) -> (product . (map (\draw -> count draw)) . getMaximalDraws) rounds