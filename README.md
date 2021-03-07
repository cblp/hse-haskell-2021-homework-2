# Домашнее задание №2

Дедлайн: 28 марта.

## RWS = Reader + Writer + State

### 1. instance Functor для

```hs
newtype RWS r w s a = RWS {runRWS :: r -> s -> (a, s, w)}
```

RWS имеет семантику, аналогичную Reader по параметру _r_, Writer по _w_ (который может быть моноидом), State по _s_.

Нельзя пользоваться _DeriveFunctor_.

### 2. instance Applicative RWS

### 3. instance Monad RWS

### 4. Просмотр окружения Reader

```hs
ask :: RWS r w s r
```

### 5. Вычисление в модифицированном окружении Reader

```hs
local :: (r -> r) -> RWS r w s a -> RWS r w s a
```

### 6. Вывод во Writer

```hs
tell :: w -> RWS r w s ()
```

### 7. Запуск временного Writer и получение результата

```hs
listen :: RWS r w s a -> RWS r w s (a, w)
```

### 8. Получение состояние State

```hs
get :: RWS r w s s
```

### 9. Запись состояния State

```hs
put :: s -> RWS r w s ()
```

<!--
## Стрелки

Реализовать категорию стрелок, позволяющих хранить информацию о прошлом и неявно работающих со временем (с временной дельтой как с `Double`), и инстансы `Category`, `Arrow`. Определение стрелок вы найдёте в модуле `Control.Arrow` в `base`. Поизучайте первый слитый вариант задания.

### 10

```haskell
data SignalFunction a b = _
```

### 11

```haskell
instance Category SignalFunction
```

### 12

```haskell
instance Arrow SignalFunction
```

### 13. Интегрирование

Реализовать стрелку, интегрирующую ломаную с узлами $(t_i, x_i)$.

```haskell
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

```haskell
someFunction :: SignalFunction (Double, Double) (Double, Double)
someFunction = _
```

2.4 Написать функцию, запускающую схему на списке входных данных и временных дельт.

```haskell
runSignalFunction someFunction (0, 0) [ ((1, 2), 0.1)
                                      , ((3, 4), 0.2)
                                      , ((5, 6), 0.3) ] =
                                      [ (0.4, 8)
                                      , (3, 18)
                                      , (9.9, 28) ]
```

Аргументы - стрелка, значение в момент времени 0, список входных данных, сопровождаемых временной дельтой.
```haskell
runSignalFunction :: SignalFunction a b -> a -> [(a, Double)] -> [b]
runSignalFunction sf atZero inputs = outputs
  where
  outputs = _
```
-->

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

```haskell
data Fork = ForkAcquired | ForkReleased
data Forks -- = ?
```

Сигнал переключающий философа
```haskell
data PhilosopherSignal = Hungry | Full
Instance Show PhilosopherSignal
instance Exception PhilosopherSignal
```

Любая специфичная для конкретного философа информация.
```haskell
data PhilosopherState = ?
```

3.2.* Написать базовый функционал.

```haskell
-- Отправляем сигналы
makeHungry :: ThreadId -> IO ()
makeHungry = _

makeFull :: ThreadId -> IO ()
makeFull = _
```

Поток, имитирующий философа. Первый аргумент - часть общего состояния. Философ должен обрабатывать исключения типа PhilosopherSignal и переключать своё поведение.
```haskell
philosopher :: PhilosopherState -> IO ()
philosopher = _
```

Функция возвращающая ссылки на потоки.
```haskell
makePhilosophers :: IO [ThreadId]
makePhilosophers = _
```

#### №4 Задача на continuation passing style

Рассмотрим тип `MyCont r a`, параметризованный типами `r` и `a`. Данный тип - обертка над типов функции `(a -> r) -> r`. Что в функциональном программировании еще часто называется монадой `Cont`, предназначенной для так называемых функций с продолжением.

```haskell
newtype MyCont r a
  = MyCont { runCont :: (a -> r) -> r }
```

Функция с продолжением - это функция, которая хранит текущее состояние программы для дальнейшей его передачи следующему шагу вычисления. В Haskell продолжение - это функция  `a -> r`,  преобразование текущего значения в значение типа `r`.

Рассмотрим примеры обычных функций:
```haskell
square :: Int -> Int
square x = x*x

incr :: Int -> Int
incr x = x+1

func :: Int -> Int
func x = square (incr x)
```

А теперь в стиле продолжений:
```haskell
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
```haskell
instance Functor (MyCont r) where
```

4.2 Реализовать инстанс аппликатива для типа MyCont r

```haskell
instance Applicative (MyCont r) where
```

4.3 Реализовать инстанс монады для типа MyCont r

```haskell
instance Monad (MyCont r) where
```

4.4.* Реализовать функцию `callCC`, имеющую следующую сигнатуру, проверить её на функции squareCC ниже (дополнительный пункт задания, +1 балл поверх основной оценки за работу):
```haskell
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
```haskell
runCont (squareCC 5) == “120”
runCont (squareCC 15) == “too much, mate”
```

#### №5 Трасформеры монад

Рассмотрим класс `MonadTrans`. MonadTrans позволяет делать новую монаду из существующей монады, вкладывая в новую монаду все вычисления и действия из предыдущей монады. Такой способ формирования новых монад называется трансформером монад,
и задается классом `MonadTrans`:

```haskell
class MonadTrans n where
  lift :: Monad m => m a -> n m a
```
MonadTrans -- это класс с единственным методом, который берет значение в монаде `m` и посылает его в новую монаду `n`.

Реализовать инстанс `MonadTrans` для следующих типов

5.1. `MaybeT`

```haskell
newtype MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
```

5.2. ContT

```haskell
newtype ContT r m a
  = ContT { runContT :: (a -> m r) -> m r }

instance MonadTrans (ContT r) where
```

5.3. StateT

```haskell
newtype StateT s m a
  = StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
```
