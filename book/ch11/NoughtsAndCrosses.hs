module NoughtsAndCrosses where

import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player = O
            | B
            | X
            deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
         os = length (filter (== O) ps)
         xs = length (filter (== X) ps)
         ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
           line = all (== p)
           rows = g
           cols = transpose g
           dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

-- display functions
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

putGrid :: Grid -> IO ()
putGrid = putStrLn
        . unlines
        . concat
        . interleave bar
        . map showRow
        where
        bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside
        . interleave bar
        . map showPlayer
        where
        beside = foldr1 (zipWith (++))
        bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

-- interleave a value between each element in the list
interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i =  0 <= i
          && i < size^2
          && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i
             then [chop size (xs ++ [p] ++ ys)]
             else []
             where (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- read natural number from the keyboard
getNat :: String -> IO Int
getNat  prompt = do putStr prompt
                    xs <- getLine
                    if xs /= [] && all isDigit xs
                    then return (read xs)
                    else do putStrLn "ERROR: Invalid number"
                            getNat prompt

-- game stuff - human vs. human
noughtsAndCrosses :: IO ()
noughtsAndCrosses = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1, 1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g = putStrLn "It's a draw!\n"
         | otherwise = do i <- getNat (prompt p)
                          case move g i p of
                            [] -> do putStrLn "ERROR: Invalid move!"
                                     run' g p
                            [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

-- -- game stuff - human vs. computer
data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0..((size^2) - 1)]]

-- -- prune the tree to a certain depth to save time and space
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

-- minimax algorithm
--  * leaves are labelled with the winning player if there is one, or blank player
--  * other nodes are labelled with minimum or maxiumum of the player labels from
--      the child nodes one level down, depending on whose turn it is: on O's turn
--      we take minimum of the child labels, on X's turn we take maxiumum
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
                  where
                    ts' = map minimax ts
                    ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts
                        , p' == best]
               where
                 tree = prune depth (gametree g p)
                 Node (_, best) ts = minimax tree

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1, 1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O won!\n"
  | wins X g = putStrLn "Player X won!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do i <- getNat (prompt p)
                case move g i p of
                  [] -> do putStrLn "ERROR: Invalid move"
                           play' g p
                  [g'] -> play g' (next p)
  | p == X = do putStr "Player X is thinking..."
                (play $! (bestmove g p)) (next p)
{-------------------------------------------------------------------------------
  The operator $! used in the definition of the function play’ forces evaluation
  of the best move for the computer player prior to the function play being
  invoked again, without which there may be a delay between clearing the screen
  and displaying the grid in play while the best move was then calculated under
  lazy evaluation.
-------------------------------------------------------------------------------}
