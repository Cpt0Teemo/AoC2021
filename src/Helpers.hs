module Helpers where
import GHC.Base (Any)

linesAndCastAsInt :: String -> [Int]
linesAndCastAsInt = map (\x -> read x :: Int) . lines

reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []

data Cond a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y