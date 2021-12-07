module Helpers where
import GHC.Base (Any)

linesAndCastAsInt :: String -> [Int]
linesAndCastAsInt = map (\x -> read x :: Int) . lines

reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []