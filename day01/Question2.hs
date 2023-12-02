{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative

newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}
 
parseChar :: Char -> Parser Char
parseChar c = Parser f
    where
        f :: String -> Maybe (Char, String)
        f [] = Nothing
        f (x:xs) 
          | x == c = Just (c, xs)
          | otherwise = Nothing

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

nameToDigit :: String -> Char
nameToDigit "zero" = '0'
nameToDigit "one" = '1'
nameToDigit "two" = '2'
nameToDigit "three" = '3'
nameToDigit "four" = '4'
nameToDigit "five" = '5'
nameToDigit "six" = '6'
nameToDigit "seven" = '7'
nameToDigit "eight" = '8'
nameToDigit "nine" = '9'
nameToDigit _ = error "Invalid digit"

simpleDigitParser :: Parser Char
simpleDigitParser = parseChar '0'
                <|> parseChar '1'
                <|> parseChar '2'
                <|> parseChar '3'
                <|> parseChar '4'
                <|> parseChar '5'
                <|> parseChar '6'
                <|> parseChar '7'
                <|> parseChar '8'
                <|> parseChar '9'

simpleDigitNameParser :: Parser String
simpleDigitNameParser = parseString "zero"
                    <|> parseString "one"
                    <|> parseString "two"
                    <|> parseString "three"
                    <|> parseString "four"
                    <|> parseString "five"
                    <|> parseString "six"
                    <|> parseString "seven"
                    <|> parseString "eight"
                    <|> parseString "nine"

simpleReverseDigitNameParser :: Parser String
simpleReverseDigitNameParser = (parseString . reverse) "zero"
                           <|> (parseString . reverse) "one"
                           <|> (parseString . reverse) "two"
                           <|> (parseString . reverse) "three"
                           <|> (parseString . reverse) "four"
                           <|> (parseString . reverse) "five"
                           <|> (parseString . reverse) "six"
                           <|> (parseString . reverse) "seven"
                           <|> (parseString . reverse) "eight"
                           <|> (parseString . reverse) "nine"

digitParser :: Parser Char
digitParser = simpleDigitParser
          <|> nameToDigit <$> simpleDigitNameParser

reverseDigitParser :: Parser Char
reverseDigitParser = simpleDigitParser
      <|> (nameToDigit . reverse) <$> simpleReverseDigitNameParser

parseUntilFound :: Parser Char -> String -> Char
parseUntilFound parser [] = error "No digit in code!"
parseUntilFound parser input = do
    case runParser parser input of
        Nothing -> parseUntilFound parser $ tail input
        Just (x, input'') -> x

parseUntilDigitIsFound :: String -> Char
parseUntilDigitIsFound = parseUntilFound digitParser

parseUntilReverseDigitIsFound :: String -> Char
parseUntilReverseDigitIsFound = (parseUntilFound reverseDigitParser) . reverse

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
                . (apply2 parseUntilDigitIsFound parseUntilReverseDigitIsFound)
    print answer