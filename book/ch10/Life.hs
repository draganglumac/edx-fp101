module Life where

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type Board = [Pos]

glider :: Board
glider = [ (4, 2)
         , (2, 3)
         , (4, 3)
         , (3, 4)
         , (4, 4) ]

showCells :: Board -> IO ()
showCells b = sequence_ [ writeAt p "O" | p <- b ]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbours :: Pos -> [Pos]
neighbours (x, y) = map wrap [ (x-1, y-1), (x, y-1)
                             , (x+1, y-1), (x-1, y)
                             , (x+1, y), (x-1, y+1)
                             , (x, y+1), (x+1, y+1) ]

wrap :: Pos -> Pos
wrap (x, y) = (((x-1) `mod` width) + 1
              ,((y-1) `mod` height) + 1)

liveNeighbours :: Board -> Pos -> Int
liveNeighbours b = length . filter (isAlive b) . neighbours

survivors :: Board -> [Pos]
survivors b = [ p | p <- b, elem (liveNeighbours b p) [2, 3] ]

births :: Board -> [Pos]
births b = [ p | p <- rmdups (concat (map neighbours b))
                    , isEmpty b p
                    , liveNeighbours b p == 3 ]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextGen :: Board -> Board
nextGen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showCells b
            wait 500000
            life (nextGen b)

wait :: Int -> IO ()
wait n = sequence_ [ return () | _ <- [1..n] ]

main :: IO ()
main = life glider
