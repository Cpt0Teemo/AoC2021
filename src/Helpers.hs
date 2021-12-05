module Helpers where
import GHC.Base (Any)

linesAndCastAsInt :: String -> [Int]
linesAndCastAsInt = map (\x -> read x :: Int) . lines

reverseList :: [Int] -> [Int]
reverseList = foldl (flip (:)) []