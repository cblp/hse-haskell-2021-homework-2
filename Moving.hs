-- | 9
module Moving where

{-
import           Control.Monad.State
-}
import           Test.QuickCheck

moving :: Int -> [Double] -> [Double]
moving = undefined

prop_moving_example_1 :: Property
prop_moving_example_1 =
  property True .||. -- УБРАТЬ ЗАГЛУШКУ
  moving 4 [1, 5, 3, 8, 7, 9, 6] === [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]

prop_moving_example_2 :: Property
prop_moving_example_2 =
  property True .||. -- УБРАТЬ ЗАГЛУШКУ
  moving 2 [1, 5, 3, 8, 7, 9, 6] === [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
