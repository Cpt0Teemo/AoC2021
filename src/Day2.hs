{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day2 (p1, p2) where
import Helpers
import Data.List

type Location = (Int, Int)
type LocationAim = (Int, Int, Int)

p1 :: String -> Int
p1 = multiply . foldl' updateLocation (0, 0) . lines

p2 :: String -> Int 
p2 = multiply' . foldl' updateLocationWithAim (0, 0, 0) . lines  

multiply (x, y) = x * y
multiply' (x, y, _) = x * y

updateLocation :: Location -> String -> Location
updateLocation (depth, forward) str = 
    case instruction of
        "forward" -> (depth, forward + amount)
        "up" -> (depth - amount, forward)
        "down" -> (depth + amount, forward)
    where
        [instruction, value] = words str
        amount = read value

updateLocationWithAim :: LocationAim -> String -> LocationAim
updateLocationWithAim (depth, forward, aim) str = 
    case instruction of
        "forward" -> (depth + (aim * amount), forward + amount, aim)
        "up" -> (depth, forward, aim - amount)
        "down" -> (depth, forward, aim + amount)
    where
        [instruction, value] = words str
        amount = read value
