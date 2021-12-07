module Day3 (p1, p2, toInt) where
import Data.List
import Debug.Trace

type Binary = [Bool]


p1 :: String -> Int
p1 = multiplyWithComplement . getMostCommonForAll (uncurry (<)) False . map toBinary . lines

p2 :: String -> Int
p2 = multiplyBoth . map toBinary . lines
    where
        multiplyBoth binaries = product . map ($ binaries) $ [oxygenGen, carbonScrub]
        oxygenGen = toInt . getMostCommonForAll (uncurry (<=)) True
        carbonScrub = toInt . getMostCommonForAll (uncurry (>)) True

getComplement :: Binary -> Binary
getComplement = map not

toBinary :: String -> Binary
toBinary = map (== '1')

toInt :: Binary -> Int
toInt bin = foldl' (\acc (i, bit) -> if bit then acc + 2^i else acc) 0 indexedBin
    where
        indexedBin = zip [length bin - 1, length bin -2..0] bin

multiplyWithComplement :: Binary -> Int
multiplyWithComplement value = (toInt value) * (toInt $ getComplement value)

getMostCommonForAll :: ((Int, Int) -> Bool) -> Bool -> [Binary] -> Binary
getMostCommonForAll compare isReductive values = fst $ foldl' foldFn ([], values) indices
    where
        foldFn (acc, possibilities) index =
            let newAcc = if length possibilities == 1 then head possibilities else acc ++ [compareColumn (possibilities, index)]
                filteredPossibilities = filter (newAcc `isPrefixOf`) possibilities
                newPossibilities = if null filteredPossibilities || not isReductive then possibilities else filteredPossibilities
            in
                (newAcc, newPossibilities)
        indices = [0..length (head values)-1]
        compareColumn = compare . getBinaryCount . uncurry getColumn

getColumn :: [Binary] -> Int -> Binary
getColumn values col = map (!!col) values

getBinaryCount :: Binary -> (Int, Int)
getBinaryCount = foldl' (\(zeros, ones) x -> if x then (zeros, ones+1) else (zeros+1, ones)) (0, 0)



