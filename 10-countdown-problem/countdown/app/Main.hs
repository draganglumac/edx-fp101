module Main where

import Countdown

main :: IO ()
main = do
  -- Brute-force solution
  putStrLn "Brute-force"
  countdown solveBrute
  -- First optimisation, eliminating invalid expressions early
  putStrLn "O1"
  countdown solveO1
  -- First optimisation, commutativity of multiplication and addition and
  -- 1 as the neutral element for multiplication and division
  putStrLn "O2"
  countdown solveO2
