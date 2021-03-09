module RWS where

{-
import           Test.QuickCheck
import           Test.QuickCheck.Poly
-}

newtype RWS r w s a = RWS{runRWS :: r -> s -> (a, s, w)}

-- | 20
instance Functor (RWS r w s) where
  fmap = undefined

-- | 21
instance Applicative (RWS r w s) where
  pure = undefined
  _ <*> _ = undefined

-- | 22
instance Monad (RWS r w s) where
  _ >>= _ = undefined

-- | 23. Построение простого ридера
reader :: (r -> a) -> RWS r w s a
reader = undefined

-- | 24. Просмотр окружения
ask :: RWS r w s r
ask = undefined

-- | 25. Вычисление в модифицированном окружении
local :: (r -> r) -> RWS r w s a -> RWS r w s a
local = undefined

-- | 26. Построение простого райтера
writer :: (a, w) -> RWS r w s a
writer = undefined

-- | 27. Вывод в Writer
tell :: w -> RWS r w s ()
tell = undefined

-- | 28. Запуск временного Writer и получение результата
listen :: RWS r w s a -> RWS r w s (a, w)
listen = undefined

-- | 29. Построение простого State-действия
state :: (s -> (a, s)) -> RWS r w s a
state = undefined

-- | 30. Чтение
get :: RWS r w s s
get = undefined

-- | 31. Запись
put :: s -> RWS r w s ()
put = undefined
