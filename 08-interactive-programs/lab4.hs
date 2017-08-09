module Lab4 where

------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

import Data.Char

-- ===================================
-- Ex. 0
-- ===================================

triangle :: Integer -> Integer
triangle n = if n < 0 then undefined else go 0 0
  where go x acc
          | x > n     = acc
          | otherwise = go (x + 1) (x + acc)


-- ===================================
-- Ex. 1
-- ===================================

count :: Eq a => a -> [a] -> Int
count x xs = go x xs 0
  where go a as acc = case as of
          []       -> acc
          (x : xs) -> if x == a
                      then go a xs (acc + 1)
                      else go a xs acc

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]

-- ===================================
-- Ex. 2
-- ===================================

euclid :: (Int,  Int) -> Int
euclid (x, y)
  | x <= 0 = undefined
  | y <= 0 = undefined
  | x > y  = euclid (y, x - y)
  | x < y  = euclid (x, y - x)
  | x == y = x

-- ===================================
-- Ex. 3
-- ===================================

funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap f g xs =
  case xs of
    [] -> []
    [x] -> [f x]
    (x : y : ys) -> (f x) : (g y) : funkyMap f g ys
