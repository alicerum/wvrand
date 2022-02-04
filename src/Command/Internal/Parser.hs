{-# LANGUAGE LambdaCase #-}

module Command.Internal.Parser (
    Parser (Parser, runParser),
    char,
    integer,
    eof
) where

import Data.Char
import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser pf) = Parser $ \s -> do
        (a, s') <- pf s
        return (f a, s')

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    (Parser f) <*> (Parser p) = Parser $ \s -> do
        (ff, s1) <- f s
        (pp, s2) <- p s1
        return (ff pp, s2)

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \s -> do
        (a, s1) <- p s
        runParser (f a) s1

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \s ->
        p1 s <|> p2 s

char :: Char -> Parser Char    
char c = Parser $ \case
    (x:cs) | x == c -> Just (x, cs)
    _ -> Nothing

digit :: Parser Char
digit = Parser $ \case
    (x:cs) | isDigit x -> Just (x, cs)
    _ -> Nothing

integer :: Parser Int
integer = read <$> some digit

eof :: Parser ()
eof = Parser $ \case
    [] -> Just ((), "")
    _ -> Nothing

