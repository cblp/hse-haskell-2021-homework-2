module NonEmpty where

import           Control.Applicative
import           Data.Foldable
import           Test.QuickCheck
import           Test.QuickCheck.Poly

data NonEmpty a = a :| [a]
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftA2 (:|) arbitrary arbitrary

instance Functor NonEmpty where
  fmap = undefined

prop_NonEmpty_fmaps_as_a_list :: Blind (A -> B) -> NonEmpty A -> Property
prop_NonEmpty_fmaps_as_a_list (Blind _f) _xs =
  -- toList (fmap f xs) === fmap f (toList xs)
  property True -- УБРАТЬ ЗАГЛУШКУ

instance Foldable NonEmpty where
  foldMap = undefined
  foldr = undefined
  toList = undefined
