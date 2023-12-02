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

digitParser :: Parser Char
digitParser = parseChar '0' 
      <|> parseChar '1' 
      <|> parseChar '2'
      <|> parseChar '3'
      <|> parseChar '4'
      <|> parseChar '5'
      <|> parseChar '6'
      <|> parseChar '7'
      <|> parseChar '8'
      <|> parseChar '9'
      <|> (nameToDigit <$> parseString "zero")
      <|> (nameToDigit <$> parseString "one")
      <|> (nameToDigit <$> parseString "two")
      <|> (nameToDigit <$> parseString "three")
      <|> (nameToDigit <$> parseString "four")
      <|> (nameToDigit <$> parseString "five")
      <|> (nameToDigit <$> parseString "six")
      <|> (nameToDigit <$> parseString "seven")
      <|> (nameToDigit <$> parseString "eight")
      <|> (nameToDigit <$> parseString "nine")

reverseDigitParser :: Parser Char
reverseDigitParser = parseChar '0' 
      <|> parseChar '1' 
      <|> parseChar '2'
      <|> parseChar '3'
      <|> parseChar '4'
      <|> parseChar '5'
      <|> parseChar '6'
      <|> parseChar '7'
      <|> parseChar '8'
      <|> parseChar '9'
      <|> ((nameToDigit . reverse) <$> (parseString . reverse) "zero")
      <|> ((nameToDigit . reverse) <$> (parseString . reverse) "one")
      <|> ((nameToDigit . reverse) <$> (parseString . reverse) "two")
      <|> ((nameToDigit . reverse) <$> (parseString . reverse) "three")
      <|> ((nameToDigit . reverse) <$> (parseString . reverse) "four")
      <|> ((nameToDigit . reverse) <$> (parseString . reverse) "five")
      <|> ((nameToDigit . reverse) <$> (parseString . reverse) "six")
      <|> ((nameToDigit . reverse) <$> (parseString . reverse) "seven")
      <|> ((nameToDigit . reverse) <$> (parseString . reverse) "eight")
      <|> ((nameToDigit . reverse) <$> (parseString . reverse) "nine")

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