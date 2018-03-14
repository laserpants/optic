module Main where

type Token = String

newtype Parser a = Parser { parse :: [Token] -> (a, [Token]) }

unit :: a -> Parser a
unit t = Parser parse where parse tokens = (t, tokens)

instance Functor Parser where 
  fmap f (Parser parse) = Parser parse' where
    parse' tokens = let (a, t) = parse tokens in (f a, t)

instance Applicative Parser where 
  Parser fun <*> Parser arg = Parser parse' where
    parse' tokens = let (f, t) = fun tokens; (x, u) = arg t in (f x, u)
  pure = unit

--

strOpt :: String -> Parser [String]
strOpt = undefined

countOpt :: String -> Parser Int
countOpt = undefined

data MyOpts = MyOpts [String] Int
  deriving (Show)

myOptsParser :: Parser MyOpts
myOptsParser = MyOpts <$> strOpt "file" <*> countOpt "verbose"

main :: IO ()
main = pure ()
