{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day1 (p1, p2) where
import Helpers (linesAndCastAsInt, reverseList)

p1 :: String -> Int
p1 = countIncreases 0 . linesAndCastAsInt

p2 :: String -> Int 
p2 = countIncreases 0 . groupInThree . linesAndCastAsInt

countIncreases :: Int -> [Int] -> Int
countIncreases _ [] = 0
countIncreases count [_] = count
countIncreases count (x:y:tail)
    | y > x = countIncreases (count+1) (y:tail)
    | otherwise = countIncreases count (y:tail)

groupInThree :: [Int] -> [Int]
groupInThree = groupInThreeRec [] . reverseList
    where
        groupInThreeRec output [_, _] = output
        groupInThreeRec output (x:y:z:tail) = groupInThreeRec ((x+y+z):output) (y:z:tail)