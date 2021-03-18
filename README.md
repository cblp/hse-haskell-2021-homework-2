# Домашнее задание №2

Мягкий дедлайн: 25 марта, 23:59 MSK.
Жесткий дедлайн: 28 марта, 23:59 MSK.

Сдавать в https://classroom.github.com/a/EmDnGRXO коммитами в master.

## Оценка

За каждую пронумерованную задачу — 1 балл.

На десятку достаточно решить 25 задач (набрать 25 баллов).

## Требования

Алгоритмы должны быть асимптотически оптимальными по времени.

Нельзя пользоваться частичными функциями (head, tail, NonEmpty.fromList).
<!-- без необходимости. -->

## Тесты

Ко всем функциям строго желательно писать юнит-тесты (кейсы и проперти).

Тесты свойств (property) — это тесты со случайно выбираемыми параметрами.

В качестве кейс-тестов можно брать проперти без параметров.

```hs
-- кейс-тесты де факто, потому что единственный случай тестируется
prop_example1 :: Property
prop_example1 = 2 + 2 === 4

prop_example2 :: Bool
prop_example2 = null emptyTree

-- настоящие проперти — со случайно выбираемыми параметрами
prop_example3 :: NonEmpty A -> Property
prop_example3 xs = head (sort xs) === minimum xs

prop_example4 :: NonEmpty A -> Bool
prop_example4 xs = not $ null xs
```

Тесты для инстансов можно писать на основе законов классов:

```hs
prop_Functor_Identity :: f A -> Property
prop_Functor_Identity x =
  fmap id x === x

prop_Functor_Composition :: Fun B C -> Fun A B -> f A -> Property
prop_Functor_Composition (Fun _ f) (Fun _ g) x =
  fmap (f . g) x === (fmap f . fmap g) x

prop_Applicative_Identity :: f A -> Property
prop_Applicative_Identity v =
  (pure id <*> v) === v

prop_Applicative_Composition :: f (Fun B C) -> f (Fun A B) -> f A -> Property
prop_Applicative_Composition u' v' w =
  (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))
  where
    u = applyFun <$> u'
    v = applyFun <$> v'

prop_Applicative_Homomorphism :: Fun A B -> A -> Property
prop_Applicative_Homomorphism (Fun _ f) x =
  (pure f <*> pure x) === (pure (f x) :: f B)

prop_Applicative_Interchange :: f (Fun A B) -> A -> Property
prop_Applicative_Interchange u' y =
  (u <*> pure y) === (pure ($ y) <*> u)
  where
    u = applyFun <$> u'

prop_Monad_LeftIdentity :: A -> Fun A (m B) -> Property
prop_Monad_LeftIdentity a (Fun _ k) =
  (return a >>= k) === k a

prop_Monad_RightIdentity :: m B -> Property
prop_Monad_RightIdentity m =
  (m >>= return) === m

prop_Monad_Associativity :: f A -> Fun A (f B) -> Fun B (f C) -> Property
prop_Monad_Associativity m (Fun _ k) (Fun _ h) =
  (m >>= (\x -> k x >>= h)) === ((m >>= k) >>= h)

prop_traverse_Identity :: t A -> Property
prop_traverse_Identity x =
  traverse Identity x === Identity x

prop_traverse_Composition :: Fun A (F B) -> Fun B (G C) -> t A -> Property
prop_traverse_Composition (Fun _ f) (Fun _ g) x =
  traverse (Compose . fmap g . f) x
    === (Compose . fmap (traverse g) . traverse f) x

prop_sequenceA_Identity :: t A -> Property
prop_sequenceA_Identity x =
  (sequenceA . fmap Identity) x === Identity x

prop_sequenceA_Composition :: t (F (G A)) -> Property
prop_sequenceA_Composition x =
  (sequenceA . fmap Compose) x === (Compose . fmap sequenceA . sequenceA) x

type F = Maybe

type G = Either String
```

## Functor и его друзья

### 1. Сумма чисел в строке

```hs
stringSum :: String -> Maybe Int
```

Числа в строке разделены одним или несколькими пробельными символами. Если хотя бы один элемент строки нельзя сконвертировать в целое число, то необходиомо вернуть `Nothing`.

Функция должна использовать инстанс Traversable для списка.

### 2. Тесты

Написать несколько простых проперти тестов на _stringSum_.

### 3—7. Инстансы NonEmpty

```hs
data NonEmpty a = a :| [a]
```

Вручную без deriving.

3. Functor
4. Applicative
5. Monad
6. Foldable
7. Traversable

## Монады и монадические вычисления

### 8. Арифметическое выражение

Арифметическое выражение (именно выражение, не результат его вычисления) можно представить рекурсивным алгебраическим типом данных. Реализуйте этот тип данных, чтобы с его помощью можно было задавать следующие операции:

* Целочисленные константы
* Сложение двух выражений
* Вычитание выражений
* Произведение выражений
* Деление выражений
* Возведение в степень выражений

После этого напишите функцию, которая принимает выражение и вычисляет его. Обратите внимание на то, что выражение может не получиться вычислить по разным причинам.

```hs
eval :: Expr -> Either ArithmeticError Int
```

То есть Вы должны создать свой тип данных, который обозначает арифметическую ошибку и возвращать `Either` — либо ошибка, которая возникла, либо результат. Если выражение содержит несколько ошибок, то можно вернуть любую.

Достаточно проверять только на следующие арифметические ошибки:

1. Деление на 0.
2. Возведение в отрицательную степень.

**Подсказка:** если реализовать функцию с `Either` сразу тяжело, то попробуйте `eval :: Expr -> Maybe Int`, после чего замените `Maybe` на `Either String`, а затем `String` можно будет заменить за свой тип данных.

### 9. Simple Moving Average

Реализуйте [Simple Moving Average](https://en.wikipedia.org/wiki/Moving_average) алгоритм, используя State. Надо придумать, как реализовать алгоритм с изменяемым значением, определив, какая часть данных должна изменяться и передаваться между итерациями.

```hs
ghci> moving 4 [1, 5, 3, 8, 7, 9, 6]
[1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]

ghci> moving 2 [1, 5, 3, 8, 7, 9, 6]
[1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
```

## Парсер-комбинаторы

Это блок самый важный в этом домашнем задании. Реализация всех упражнений из этого блока поможет понять, как устроены парсер-комбинаторы, а это важно, потому что они крайне полезны на практике. Перед решением заданий убедитесь, что вы осознали материал семинара и можете прорешать базовые упражнения по следующим ссылкам:

* [Parser Combinators: Basics](http://www.seas.upenn.edu/~cis194/spring13/hw/10-applicative.pdf)
* [Parser Combinators: Implementing simple parser](http://www.seas.upenn.edu/~cis194/spring13/hw/11-applicative2.pdf)

### 10—13

Имеется тип простого парсера:

```hs
newtype Parser s a = Parser { runParser :: [s] -> Either String (a, [s]) }
```

Он может работать не только со строкой (s ~ Char), но и с любым потоком данных.
В Left должно быть человеческое описание ошибки.

Реализуйте вручную инстансы:

10. Functor
11. Applicative
12. Monad
13. Alternative

### 14—17. Базовые парсеры

14. ok — Парсер никогда не падает и не поглощает инпут.
15. eof — Проверяет, что парсер дошёл до конца потока данных (иначе падает).
16. satisfy — Парсер принимает предикат на элемент потока и возвращает элемент, поглащая его из потока, если предикат на элемент равен `True`, иначе падает.
17. element и stream — Парсят один или несколько элементов потока (как `char` и `string`).

### 18—19. Простые парсеры

Используя существующие комбинаторы (реализовав по необходимости остальные), напишите следующие парсеры строковых потоков:

18. Парсер правильных скобочных последовательностей (падает, если последовательность неправильная, и не падает, если правильная).
19. Парсер целого числа, перед которым может быть знак `+` или `-`. Нельзя пользоваться функциями, связанными с классом Read.

<!--
### 20. Непростой парсер

Написать парсер списка списков чисел, разделённых запятой. Парсер должен иметь тип:

```hs
listListParser :: Parser Char [[Int]]
```

Все числа перечисленны через запятую. В начале каждого списка находится число — длина списка. Таким образом можно понять, где заканчивается каждый список. То есть список

```hs
[ [1, 10], [5, -7, 2] ]
```

в строковом виде может быть записан следующим образом:

```hs
"2, 1,+10  , 3,5,-7, 2"
```
-->

## RWS = Reader + Writer + State

### 20—22. Инстансы для RWS

```hs
newtype RWS r w s a = RWS {runRWS :: r -> s -> (a, s, w)}
```

RWS имеет семантику, аналогичную Reader по параметру _r_, Writer по _w_ (который может быть моноидом), State по _s_.

Реализовать истансы без deriving:

20. Functor
21. Applicative
22. Monad

### 23—25. Полезные ридеры

```hs
-- | 23. Построение простого ридера
reader :: (r -> a) -> RWS r w s a

-- | 24. Просмотр окружения
ask :: RWS r w s r

-- | 25. Вычисление в модифицированном окружении
local :: (r -> r) -> RWS r w s a -> RWS r w s a
```

### 26—28. Полезные райтеры

```hs
-- | 26. Построение простого райтера
writer :: (a, w) -> RWS r w s a

-- | 27. Вывод во Writer
tell :: w -> RWS r w s ()

-- | 28. Запуск временного Writer и получение результата
listen :: RWS r w s a -> RWS r w s (a, w)
```

### 29—31. Полезные State-действия

```hs
-- | 29. Построение простого State-действия
state :: (s -> (a, s)) -> RWS r w s a

-- | 30. Чтение
get :: RWS r w s s

-- | 31. Запись
put :: s -> RWS r w s ()
```

<!--
## Стрелки

Реализовать категорию стрелок, позволяющих хранить информацию о прошлом и неявно работающих со временем (с временной дельтой как с `Double`), и инстансы `Category`, `Arrow`. Определение стрелок вы найдёте в модуле `Control.Arrow` в `base`. Поизучайте первый слитый вариант задания.

### 10

```hs
data SignalFunction a b = _
```

### 11

```hs
instance Category SignalFunction
```

### 12

```hs
instance Arrow SignalFunction
```

### 13. Интегрирование

Реализовать стрелку, интегрирующую ломаную с узлами $(t_i, x_i)$.

```hs
integral :: SignalFunction Double Double
integral = _
```

### 14. Реализовать схему

Используя расширение -XArrows и arrow-нотацию

```
     +=========+
---- |   * 2   | ---
     +=========+    \   +=========+        +==========+
                     -- |    +    | ------ | integral | -----
     +=========+    /   +=========+ \      +==========+
---- |   * 3   | ---                 ------------------------
     +=========+
```

Эта схема принимает `(x, y)`, а возвращает `(I, 2x + 3y)`, где `I` - накопленная сумма под графиком ломанной.  Все вычисления должны проходить в стрелках (не должно быть let-присваиваний и  функций справа от -<)

```hs
someFunction :: SignalFunction (Double, Double) (Double, Double)
someFunction = _
```

2.4 Написать функцию, запускающую схему на списке входных данных и временных дельт.

```hs
runSignalFunction someFunction (0, 0) [ ((1, 2), 0.1)
                                      , ((3, 4), 0.2)
                                      , ((5, 6), 0.3) ] =
                                      [ (0.4, 8)
                                      , (3, 18)
                                      , (9.9, 28) ]
```

Аргументы - стрелка, значение в момент времени 0, список входных данных, сопровождаемых временной дельтой.
```hs
runSignalFunction :: SignalFunction a b -> a -> [(a, Double)] -> [b]
runSignalFunction sf atZero inputs = outputs
  where
  outputs = _
```
-->

<!--
#### №3* Задачи на Concurrency (дополнительная задача, дающая +2.5 балла поверх основной оценки)

Используя библиотеку STM, и модули Control.Concurrent, Control.Exception из base
решить задачу "Обедающих философов".

У нас есть 5 философов, предоставляемые отдельными потоками, и 5 вилок между ними, которые философы могут брать в руки и отпускать. Каждый философов может размышлять или обедать, его желания переключаются отправкой сигнала в поток. Если философ хочет есть и рядом с ним есть две свободные вилки, он берёт их в руки и ест. Необходимо добиться, чтобы философы не блокировали другу друга.

Пример блокировки:
1. Первый философ ест.
2. Второй философ проголодался, но рядом всего одна свободная вилка. Он берёт её в руки, но не может начать есть пока другая занята.
3. Третий философ проголодался, но второй сам не ест и ему не даёт.

Пример dead-lock'а:
1. Каждый философ берёт вилку, а потом ещё одну вилку. Из-за несогласованности
получилось, что каждый философ оказался с одной вилкой в руках и никто не может начать есть.

3.1.* Написать необходимые типы. Состояние храним с помощью монады STM.

```hs
data Fork = ForkAcquired | ForkReleased
data Forks -- = ?
```

Сигнал переключающий философа
```hs
data PhilosopherSignal = Hungry | Full
Instance Show PhilosopherSignal
instance Exception PhilosopherSignal
```

Любая специфичная для конкретного философа информация.
```hs
data PhilosopherState = ?
```

3.2.* Написать базовый функционал.

```hs
-- Отправляем сигналы
makeHungry :: ThreadId -> IO ()
makeHungry = _

makeFull :: ThreadId -> IO ()
makeFull = _
```

Поток, имитирующий философа. Первый аргумент - часть общего состояния. Философ должен обрабатывать исключения типа PhilosopherSignal и переключать своё поведение.
```hs
philosopher :: PhilosopherState -> IO ()
philosopher = _
```

Функция возвращающая ссылки на потоки.
```hs
makePhilosophers :: IO [ThreadId]
makePhilosophers = _
```
-->

<!--
#### №4 Задача на continuation passing style

Рассмотрим тип `MyCont r a`, параметризованный типами `r` и `a`. Данный тип - обертка над типов функции `(a -> r) -> r`. Что в функциональном программировании еще часто называется монадой `Cont`, предназначенной для так называемых функций с продолжением.

```hs
newtype MyCont r a
  = MyCont { runCont :: (a -> r) -> r }
```

Функция с продолжением - это функция, которая хранит текущее состояние программы для дальнейшей его передачи следующему шагу вычисления. В Haskell продолжение - это функция  `a -> r`,  преобразование текущего значения в значение типа `r`.

Рассмотрим примеры обычных функций:
```hs
square :: Int -> Int
square x = x*x

incr :: Int -> Int
incr x = x+1

func :: Int -> Int
func x = square (incr x)
```

А теперь в стиле продолжений:
```hs
squareCps :: Int -> (Int -> r) -> r
squareCps x f = f (x*x)

incrCps :: Int -> (Int -> r) -> r
incrCps x f = f (x+1)

funcCps :: Int -> (Int -> r) -> r
funcCps x f = incrCps x $ \inc ->
               squareCps inc $ \sq ->
               f sq
```
В первых двух функциях продолжение применяется к результату возведения в квадрат и к результату прибавления единицы соответственно. Третья функция - это реализованная через cps композиция cps-квадрата и cps-инкремента. По факту, этот пример показывает, как компоновать cps-функции. При реализации инстанса монады для `MyCont` вы увидите, что функцию, аналогичную `funcCps`, достаточно легко реализовать с использованием do-нотации. Нетрудно также заметить, что функции `squareCps`, `incrCps` и `funcCps` дают в частном случае функции `square`, `incr`, и `func`, если в качестве продолжений передать функцию `id`, которая возвращает аргумент, ничего с ним не делая.

Более подробное объяснение функций с продолжением смотрите в этом блог-посте:
http://www.haskellforall.com/2012/12/the-continuation-monad.html


4.1 Реализовать инстанс функтора для типа MyCont r
```hs
instance Functor (MyCont r) where
```

4.2 Реализовать инстанс аппликатива для типа MyCont r

```hs
instance Applicative (MyCont r) where
```

4.3 Реализовать инстанс монады для типа MyCont r

```hs
instance Monad (MyCont r) where
```

4.4.* Реализовать функцию `callCC`, имеющую следующую сигнатуру, проверить её на функции squareCC ниже (дополнительный пункт задания, +1 балл поверх основной оценки за работу):
```hs
callCC :: ((a -> MyCont r b) -> MyCont r a) -> MyCont r a
callCC = undefined
```
Функция `callCC` довольно часто используется для быстрого обрыва вычислений.
Например:
```
squareCC :: Int -> MyCont r String
squareCC n = callCC $ \exit -> do
  let fac = product [1..n]
  when (n > 10) $ exit “too much, mate”
  return $ show fac
```
Несколько тест-кейсов для проверки функции `callCC`
```hs
runCont (squareCC 5) == “120”
runCont (squareCC 15) == “too much, mate”
```
-->

<!--
## Трасформеры монад

Реализовать инстанс `MonadTrans` для следующих типов

5.1. `MaybeT`

```hs
newtype MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
```

5.2. ContT

```hs
newtype ContT r m a
  = ContT { runContT :: (a -> m r) -> m r }

instance MonadTrans (ContT r) where
```

5.3. StateT

```hs
newtype StateT s m a
  = StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
```
-->
