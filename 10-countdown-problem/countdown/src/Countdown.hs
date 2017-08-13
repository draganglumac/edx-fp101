module Countdown where

-- The game of numbers from Channel 4 show Countdown
-- All numbers are positive integers and the result is a positive integer.

data Op = Add
        | Sub
        | Mul
        | Div
        deriving (Show)

-- Apply an operator
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Rules for valid expressions
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
-- no need to check for division by 0 as all numbers are positive integers

data Expr = Val Int
          | App Op Expr Expr
          deriving (Show)

-- Evaluate an expression - return the overall value of the expression
-- providing it is a positive integer
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                 , y <- eval r
                                 , valid o x y]

subs                          :: [a] -> [[a]]
subs []                       =  [[]]
subs (x:xs)                   =  yss ++ map (x:) yss
                               where yss = subs xs
interleave                    :: a -> [a] -> [[a]]
interleave x []               =  [[x]]
interleave x (y:ys)           =  (x:y:ys) : map (y:) (interleave x ys)

perms                         :: [a] -> [[a]]
perms []                      =  [[]]
perms (x:xs)                  =  concat (map (interleave x) (perms xs))

-- all possible way of chosing 0 or more elements from the input list
--   choices [1, 2] = [[], [1], [2], [1, 2], [2, 1]]
choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs
                 , zs <- perms ys]

-- Return the list of all the values in the input expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns)
                  && eval e == [n]


-- ************************
-- * Brute-force solution *
-- ************************
--
-- split takes a list and returns a pair of lists which represent all
-- possible ways that you can split the input list into pairs of non-empty lists
-- split [1, 2, 3, 4] = [([1], [2, 3, 4]), ([1, 2], [3, 4]), ([1, 2, 3], [4])]
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

-- Return a list of all possible expressions whose values are precisely a
-- given list of numbers
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns
              , l <- exprs ls
              , r <- exprs rs
              , e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App op l r | op <- [Add, Sub, Mul, Div]]

solveBrute :: [Int] -> Int -> [Expr]
solveBrute ns n = [e | ns' <- choices ns
                    , e <- exprs ns'
                    , eval e == [n]]

countdown :: ([Int] -> Int -> [Expr]) -> IO ()
countdown solve = do
  putStrLn $ show $ take 1 (solve [1, 3, 7, 10, 25, 50] 765)
  putStrLn $ show $ take 10 (solve [1, 3, 7, 10, 25, 50] 765)


-- ****************************************************************
-- * O1 - fuse generation and evaluation to reject invalids early *
-- ****************************************************************

-- Type that caries both symbolic and evaluated expression
type Result = (Expr, Int)

-- Fuse together generation and evaluation of expressions
results :: [Int] -> [Result]
-- results ns = [(e, n) | e <- exprs ns
--                      , n <- eval e]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
  [res | (ls, rs) <- split ns
       , lx <- results ls
       , ry <- results rs
       , res <- combineO1 lx ry]

-- check if results are valid, and combine if valid
combineO1 :: Result -> Result -> [Result]
combineO1 (l, x) (r, y) =
  [(App o l r, apply o x y) | o <- [Add, Sub, Mul, Div]
                            , valid o x y]

solveO1 :: [Int] -> Int -> [Expr]
solveO1 ns n = [e | ns' <- choices ns
                      , (e, m) <- results ns'
                      , m == n]


-- ********************************************************************
-- * O2 - shrink the search space by exploiting properties of the ADT *
-- ********************************************************************
--
-- Properties we want to exploit to shrink the search space are
--
--   x + y = y + x    i.e. commutativity of addition
--   x * y = y * x    i.e. commutativity of multiplication
--   x * 1 = x        i.e. multiplication with neutral element

-- moreValid - stronger validity check
moreValid :: Op -> Int -> Int -> Bool
moreValid Add x y = x <= y
moreValid Sub x y = x > y
moreValid Mul x y = x <= y && x /= 1 && y /= 1
moreValid Div x y = x `mod` y == 0 && y /= 1

-- check if results are valid, and combine if valid
combineO2 :: Result -> Result -> [Result]
combineO2 (l, x) (r, y) =
  [(App o l r, apply o x y) | o <- [Add, Sub, Mul, Div]
                            , moreValid o x y]


-- Fuse together generation and evaluation of expressions
resultsO2 :: [Int] -> [Result]
resultsO2 [] = []
resultsO2 [n] = [(Val n, n) | n > 0]
resultsO2 ns =
  [res | (ls, rs) <- split ns
       , lx <- resultsO2 ls
       , ry <- resultsO2 rs
       , res <- combineO2 lx ry]


solveO2 :: [Int] -> Int -> [Expr]
solveO2 ns n = [e | ns' <- choices ns
                     , (e, m) <- resultsO2 ns'
                     , m == n]


-- removes the first occurence of a given element from a list
removeone :: Eq a => a -> [a] -> [a]
removeone _ [] = []
removeone a (x : xs)
  | x == a = xs
  | otherwise = removeone a xs

-- decides whether first list is chosen from the second
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x : xs) [] = False
isChoice (x : xs) ys = elem x ys && isChoice xs (removeone x ys)
