module TinyParse (
        Parser(parse),
        number,
        stringLiteral,
        string,
        sepBy,
        whiteSpace,
        item,
        char,
        character,
        escape,
        satisfy, 
        module Control.Applicative,
        module Control.Monad,
        module Data.Char        
        ) where

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }

instance Functor Parser where
        fmap f (Parser p) = 
                Parser $ \input -> do 
                                (a,input') <- p input
                                return (f a, input')

instance Applicative Parser where
        pure x = Parser $ \input -> Just (x,input)
        (Parser p1) <*> (Parser p2) =
                Parser $ \input -> do
                        (f,input')  <- p1 input
                        (x,input'') <- p2 input'
                        return (f x, input'')

instance Alternative Parser where
        empty = Parser $ \_ -> empty
        (Parser p1) <|> (Parser p2) =
                Parser $ \input -> (p1 input) <|> (p2 input)

instance Monad Parser where
        return           = pure
        (Parser p) >>= f =
                Parser $ \input -> do
                        (a,rest) <- p input
                        parse (f a) rest

item :: Parser Char
item = Parser $ 
        \input -> case input of
             []   -> Nothing
             x:xs -> return (x,xs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
        c <- item
        if pred c then return c else empty

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string = sequenceA . map char

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p  = (sep `sepBy1` p) <|> return []

sepBy1 :: Parser a -> Parser b -> Parser [b]
sepBy1 sep p = (:) <$> p <*> many (sep *> p)

character :: Parser Char
character = satisfy (liftA2 (&&) (/= '"') (/= '\\'))

escape :: Parser Char
escape  =
        ( '"' <$ string "\\\"") <|>     
        ('\\' <$ string "\\\\") <|>     
        ('/' <$ string "\\/" ) <|>      
        ('\b' <$ string "\\b" ) <|>     
        ('\f' <$ string "\\f" ) <|>     
        ('\n' <$ string "\\n" ) <|>     
        ('\r' <$ string "\\r" ) <|>     
        ('\t' <$ string "\\t" )

whiteSpace :: Parser String
whiteSpace = many (satisfy isSpace)

-- order matters here
number :: Parser Double
number = read <$> (negParse <|> decParse <|> intParse)
         where
                digit = satisfy isDigit 
                intParse = some digit
                decParse = combine <$>  intParse <*> char '.' <*> intParse
                negParse = (:) <$> char '-' <*> (decParse <|> intParse)
                
                combine :: String -> Char -> String -> String
                combine base point decimal = base ++ (point : decimal)

stringLiteral :: Parser String
stringLiteral = many (character <|> escape)
