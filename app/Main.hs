module Main where

import qualified AoC
import Paths_AoC2021

main :: IO ()
main = do
  putStrLn "Welcome to Haskel AoC 2021!"
  path <- getDataFileName "d1p1.txt"
  input <- readFile path
  print . AoC.day1p2 $ input
