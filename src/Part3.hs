module Part3 where

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 n = getSimpleDivisors 2 n == [n]

getSimpleDivisors :: Integer -> Integer -> [Integer]
getSimpleDivisors _ 1 = []
getSimpleDivisors divisor n
  | divisor * divisor > n = [n]
  | n `mod` divisor == 0 = [divisor] ++ getSimpleDivisors divisor (n `div` divisor)
  | otherwise = getSimpleDivisors (divisor + 1) n

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 n = n == sum (getDivisors n)

-- Делители числа без самого числа
getDivisors :: Integer -> [Integer]
getDivisors n = removeItem n (divisors n)

-- Удалить само число из делителей
removeItem :: Integer -> [Integer] -> [Integer]
removeItem _ [] = []
removeItem x (y : ys)
  | x == y = removeItem x ys
  | otherwise = y : removeItem x ys

divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors k =
  k :
  concatMap
    (\x -> [x] ++ if k `div` x == x then [] else [k `div` x])
    ( filter (\x -> k `mod` x == 0) $ takeWhile (\x -> x * x <= k) [2 ..])
    ++ [1]

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 n = quicksort (divisors n)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p : xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 str = product $ (map iCount) (words str)

iCount xs = toInteger (length (filter (== 'i') xs))

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 n = check 1 0
  where
    check :: Integer -> Integer -> Bool
    check current sum
        | sum == n = True
        | sum > n = False
        | otherwise = check (succ current) (sum + current)

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 n = toDigits n == (reverse . toDigits) n
    where
        toDigits :: Integer -> [Integer]
        toDigits 0 = [0]
        toDigits current = toDigitsByDiv current
            where
                toDigitsByDiv 0 = []
                toDigitsByDiv x = x `mod` 10 : toDigitsByDiv (x `div` 10)

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 n1 n2 = sumDivisors n1 == n2 && sumDivisors n2 == n1
    where sumDivisors = sum . getDivisors

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 k = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 coins mSum
    | mSum < minimum coins = []
    | otherwise = [current : nextCoins |
        current <- reverse coins,
        nextCoins <- [] : prob32 (filter (<= current) coins) (mSum - current),
        sum (current : nextCoins) == mSum]
