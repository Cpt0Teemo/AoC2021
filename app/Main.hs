module Main where

import qualified AoC
import Paths_AoC2021

main :: IO ()
main = do
  putStrLn "Welcome to Haskel AoC 2021!"
  path <- getDataFileName "d3.txt"
  input <- readFile path
  print . AoC.day3p2 $ input
