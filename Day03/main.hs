{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


-- A fun exercise to solve the problem without regex

import Data.String (IsString(..))
import Control.Applicative (Alternative(..))

-- A simple non-backtracking parser with one character of lookahead

newtype Parser a = Parser (String -> (Maybe a, String))

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \ts -> case p ts of
    (Just a, ts) -> (Just (f a), ts)
    (Nothing, ts) -> (Nothing, ts)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \ts -> (Just x, ts)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser p1) (Parser p2) = Parser $ \ts -> case p1 ts of
    (Just f, ts) -> case p2 ts of
      (Just x, ts) -> (Just (f x), ts)
      (Nothing, ts) -> (Nothing, ts)
    (Nothing, ts) -> (Nothing, ts)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \ts -> (Nothing, ts)
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser p1) (Parser p2) = Parser $ \ts -> case p1 ts of
    (Just x, ts) -> (Just x, ts)
    (Nothing, ts) -> p2 ts

findAll :: Parser a -> String -> [a]
findAll (Parser p) = \case
  [] -> []
  ts -> case p ts of
    (Nothing, ts) -> case ts of
      [] -> []
      (t:ts) -> findAll (Parser p) ts
    (Just x, ts) -> x : findAll (Parser p) ts

match :: (Char -> Bool) -> Parser Char
match p = Parser $ \ts -> case ts of
  [] -> (Nothing, [])
  (x:xs)
    | p x -> (Just x, xs)
    | otherwise -> (Nothing, x:xs)

char :: Char -> Parser Char
char c = match (== c)

string :: String -> Parser String
string = \case
  [c] -> (:[]) <$> char c
  (c:cs) -> (:) <$> char c <*> string cs

instance (a ~ String) => IsString (Parser a) where
  fromString = string
  
between :: Int -> Int -> Parser a -> Parser [a]
between n m p | m >= n && n >= 0 = between' n m where
  between' 0 0 = pure []
  between' 0 m = (:) <$> p <*> between' 0 (m - 1) <|> pure []
  between' n m = (:) <$> p <*> between' (n - 1) (m - 1)

int :: Parser Int
int = read <$> (between 1 3 digit) where
  digit = match (`elem` ['0'..'9'])

mul :: Parser (Int, Int)
mul = (,) <$> ("mul(" *> int) <*> ("," *> int <* ")")

part1 :: IO ()
part1 = do
  ts <- readFile "input.txt"
  let answer = sum $ findAll ((\(a, b) -> a * b) <$> mul) ts
  print answer

data Statement = Mul (Int, Int) | Do | Dont

part2 :: IO ()
part2 = do
  ts <- readFile "input.txt"
  let statement = (Mul <$> mul) <|> ("do" *> (("()" *> pure Do) <|> ("n't()" *> pure Dont))) 
  let answer = eval True (findAll statement ts)
  print answer
  where
    eval :: Bool -> [Statement] -> Int
    eval enabled [] = 0
    eval enabled (x:xs) = case x of
      Mul (a, b) -> (if enabled then (a * b) else 0) + eval enabled xs
      Do -> eval True xs
      Dont -> eval False xs

main :: IO ()
main = do
  part1
  part2
