module Parser where

import           Control.Applicative
{-
import           Data.Char
import           Data.Functor
import           Data.Either
import           Test.QuickCheck
import           Test.QuickCheck.Poly
-}

newtype Parser s a = Parser{runParser :: [s] -> Either String (a, [s])}

-- | 10
instance Functor (Parser s) where
  fmap = undefined

-- | 11
instance Applicative (Parser s) where
  pure = undefined
  _ <*> _ = undefined

-- | 12
instance Monad (Parser s) where
  _ >>= _ = undefined

-- | 13
instance Alternative (Parser s) where
  empty = undefined
  _ <|> _ = undefined

-- | 14
ok :: Parser s ()
ok = undefined

-- | 15
eof :: Parser s ()
eof = undefined

-- | 16
satisfy :: (s -> Bool) -> Parser s s
satisfy _p = undefined

-- | 17
element :: s -> Parser s s
element _e = undefined

stream :: [s] -> Parser s [s]
stream _s = undefined

-- | 18
balancedParentheses :: Parser Char ()
balancedParentheses = undefined

-- | 19
integer :: Parser Char Integer
integer = undefined
