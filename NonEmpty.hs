module NonEmpty where

import           Control.Applicative
import           Data.Foldable
{-
import           Data.Functor.Compose
import           Data.Functor.Identity
-}
import           Test.QuickCheck
import           Test.QuickCheck.Poly

data NonEmpty a = a :| [a]
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftA2 (:|) arbitrary arbitrary

-- | 3
instance Functor NonEmpty where
  fmap = undefined

prop_NonEmpty_fmaps_as_a_list :: Fun A B -> NonEmpty A -> Property
prop_NonEmpty_fmaps_as_a_list (Fun _ f) xs =
  property True .||. -- УБРАТЬ ЗАГЛУШКУ
  toList (fmap f xs) === fmap f (toList xs)

-- | 4
instance Applicative NonEmpty where
  pure = undefined
  _ <*> _ = undefined

-- | 5
instance Monad NonEmpty where
  _ >>= _ = undefined

-- | 6
instance Foldable NonEmpty where
  foldMap = undefined
  foldr = undefined

-- | 7
instance Traversable NonEmpty where
  traverse = undefined
  sequenceA = undefined
