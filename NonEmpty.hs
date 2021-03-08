module NonEmpty where

import           Control.Applicative
import           Data.Foldable
import           Test.QuickCheck
import           Test.QuickCheck.Poly

data NonEmpty a = a :| [a]
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftA2 (:|) arbitrary arbitrary

-- | 3
instance Functor NonEmpty where
  fmap = undefined

prop_NonEmpty_fmaps_as_a_list :: Blind (A -> B) -> NonEmpty A -> Property
prop_NonEmpty_fmaps_as_a_list (Blind _f) _xs =
  -- toList (fmap f xs) === fmap f (toList xs)
  property True -- УБРАТЬ ЗАГЛУШКУ

-- | 4
instance Applicative NonEmpty where
  pure = undefined
  _ <*> _ = undefined
  liftA2 = undefined

-- | 5
instance Monad NonEmpty where
  _ >>= _ = undefined

-- | 6
instance Foldable NonEmpty where
  foldMap = undefined
  foldr = undefined
  toList = undefined

-- | 7
instance Traversable NonEmpty where
  traverse = undefined
  sequenceA = undefined
