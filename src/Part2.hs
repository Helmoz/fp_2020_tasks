module Part2 where

import Part2.Types
import Data.Foldable
import Data.Maybe

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 l = case l of
  RED   -> 'R'
  GREEN -> 'G'
  BLUE  -> 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 p = (getInt p) >= 0 && (getInt p) <= 255

getInt :: ColorPart -> Int
getInt p = case p of
  Red   int -> int
  Green int -> int
  Blue  int -> int

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 c p = case p of
    Red   v -> Color { red = (red c) + v, green = (green c),      blue = (blue c)}
    Green v -> Color { red = (red c),     green = (green c) + v,  blue = (blue c)}
    Blue  v -> Color { red = (red c),     green = (green c),      blue = (blue c) + v}

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 p = getInt p

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 c = do
  let maxVal = getMaxVal c
  let lengthOfMaxVals = length (filter (\v -> v == maxVal) (getVals c))
  if lengthOfMaxVals > 1 then Nothing
    else getColorPartByValue c maxVal

getMaxVal :: Color -> Int
getMaxVal c = maximum (getVals c)

getVals :: Color -> [Int]
getVals c = [(red c), (green c), (blue c)]

getColorPartByValue :: Color -> Int -> Maybe ColorPart
getColorPartByValue c val
  | red   c == val = Just (Red  (red c))
  | green c == val = Just (Green (green c))
  | blue  c == val = Just (Blue (blue c))
------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 tree = (getTreeSum (left tree)) + (root tree) + (getTreeSum (right tree))

getTreeSum :: Num a => Maybe (Tree a) -> a
getTreeSum maybeTree = case maybeTree of
  Nothing -> 0
  Just val -> prob11 val

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 tree = all (< (root tree)) (getLeftValsList (left tree))
          && all (>= (root tree)) (getRightValsList (right tree))

getLeftValsList maybeTree = case maybeTree of
  Nothing -> []
  Just val -> [(root val)] ++ getLeftValsList (left val)

getRightValsList maybeTree = case maybeTree of
  Nothing -> []
  Just val -> [(root val)] ++ getRightValsList (left val)

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 num tree
  | num == (root tree) = Just tree
  | otherwise = do
      maybeLeftTree <- left tree
      maybeRightTree <- right tree
      asum [(prob13 num maybeLeftTree), (prob13 num maybeRightTree)]

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"
