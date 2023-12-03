{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Data.Char
import Data.List

data Position = Position {
      rowIdx :: Int
    , colIdx :: Int
} deriving (Show, Eq)

type ParserResult a = Position -> Maybe (a, (String, Position))

newtype Parser a = Parser {
    runParser :: String -> ParserResult a
}

data Value = Number Int Int | Dot | SpecialChar | NewLine deriving (Show)

type WithPosition a = (a, Position)

parseCharPredicate :: (Char -> Bool) -> Parser (WithPosition Char)
parseCharPredicate g = Parser f
    where 
        f :: String -> ParserResult (Char, Position)
        f [] _ = Nothing
        f (x:xs) pos
            | g x = Just ((x, pos), (xs, Position {rowIdx = rowIdx pos, colIdx = colIdx pos + 1}))
            | otherwise = Nothing
            
parseChar :: Char -> Parser (WithPosition Char)
parseChar c = parseCharPredicate $ \c' -> c == c'

instance Functor Parser where
    fmap f (Parser p) = Parser g 
        where
            g input cursorPos = do
                (c, (input', cursorPos')) <- p input cursorPos
                Just (f c, (input', cursorPos'))

instance Applicative Parser where
    pure x = Parser f 
        where 
            f input cursorPos = Just (x, (input, cursorPos))
    (<*>) (Parser f) (Parser a) = Parser g 
        where 
            g input cursorPos = do
                (f' , (input', cursorPos')) <- f input cursorPos
                (a', (input'', cursorPos'')) <- a input' cursorPos'
                Just (f' a', (input'', cursorPos''))

instance Alternative Parser where
    empty = Parser $ \input cursorPos -> Nothing
    (<|>) (Parser a) (Parser b) = Parser f
        where 
            f input cursorPos = do
                case a input cursorPos of
                    Just x -> Just x
                    Nothing -> do
                        case b input cursorPos of
                            Just y -> Just y
                            Nothing -> Nothing

parseSpan :: (Char -> Bool) -> Parser (WithPosition String)
parseSpan predicate = Parser $ f
    where 
        f :: String -> ParserResult (WithPosition String)
        f input cursorPos = do
            let (found, rest) = span predicate input 
            if found == "" then Nothing else Just ((found, cursorPos), (rest, Position (rowIdx cursorPos) ((colIdx cursorPos) + (length found))))

digitParser :: Parser (WithPosition Value)
digitParser = f <$> parseSpan isDigit
    where 
        f :: WithPosition String -> WithPosition Value
        f (chars, pos) = (Number ((read :: String -> Int) chars) (length chars), pos)

dotParser :: Parser (WithPosition Value)
dotParser = f <$> (parseSpan $ \c -> c == '.') 
    where
        f :: WithPosition String -> WithPosition Value
        f (chars, pos) = (Dot, pos)

specialCharacterParser :: Parser (WithPosition Value)
specialCharacterParser = f <$> (parseCharPredicate $ \c -> (not . isDigit) c && c /= '.' && c /= '\n')
    where
        f :: WithPosition Char -> WithPosition Value
        f (chars, pos) = (SpecialChar, pos)

newLineParser :: Parser (WithPosition Value)
newLineParser = Parser f
    where 
        f :: String -> ParserResult (WithPosition Value)
        f [] cursorPos = Nothing
        f input@(c:input') cursorPos 
            | c == '\n' = Just ((NewLine, cursorPos), (input', Position ((rowIdx cursorPos) + 1) 0))
            | otherwise = Nothing 

isNumberToken :: WithPosition Value -> Bool
isNumberToken (Number x y, pos) = True
isNumberToken _ = False

isSpecialCharToken :: WithPosition Value -> Bool
isSpecialCharToken (SpecialChar, pos) = True
isSpecialCharToken _ = False

getPosNeighbours :: Position -> [Position]
getPosNeighbours pos = do
    let rowIdx' = rowIdx pos 
    let colIdx' = colIdx pos
    [
          Position (rowIdx'-1) (colIdx'-1)
        , Position (rowIdx'-1) colIdx'
        , Position (rowIdx'-1) (colIdx'+1)
        , Position rowIdx' (colIdx'-1)
        , Position rowIdx' (colIdx'+1)
        , Position (rowIdx'+1) (colIdx'-1)
        , Position (rowIdx'+1) colIdx'
        , Position (rowIdx'+1) (colIdx'+1)
        ]

getNumberPosSpans :: Position -> Int -> [Position]
getNumberPosSpans pos len = (flip map) [0..len-1] $ \offset -> Position (rowIdx pos) (colIdx pos + offset)

positionsOverlap :: [Position] -> [Position] -> Bool
positionsOverlap positions1 positions2 = 
    any (\position1 -> any (\position2 -> position1 == position2) positions2) positions1

positionsAreNear :: Position -> Position -> Int -> Bool
positionsAreNear specialCharPos numberPos numberLen = positionsOverlap (getPosNeighbours specialCharPos) (getNumberPosSpans numberPos numberLen)

main :: IO()
main = do
    manual <- readFile "day03/input.txt"
    let maybeTokens = runParser (many ((digitParser <|> dotParser <|> specialCharacterParser <|> newLineParser)) ) manual $ Position 0 0
    case maybeTokens of
        Nothing -> error "No tokens were parsed!"
        Just (tokens, _) -> do
            let specialChars = filter isSpecialCharToken tokens
            let numbers = filter isNumberToken tokens
            print 
                . sum
                . map (\(Number n _, _) -> n )
                . filter (\(Number n nLen, nPos) -> any (\(sc, scPos) -> positionsAreNear scPos nPos nLen) specialChars) $ numbers 
