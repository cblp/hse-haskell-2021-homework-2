-- | 8
module Eval where

{-
import           Control.Applicative
import           Test.QuickCheck
-}

data Expr
  {-
  = ...
  | ...
  | ...
  -}

data ArithmeticError
  {-
  = ...
  | ...
  -}

eval :: Expr -> Either ArithmeticError Int
eval = undefined

-- prop_eval = eval ... === ...
