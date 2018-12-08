-- Day 2
import           Control.Applicative

loadInput :: IO String
loadInput = readFile "inputs/day-2.txt"

parseInput :: String -> [[Integer]]
parseInput input = map parseLine $ lines input
  where
    parseLine = map (toInteger . read) . words

checksumA :: [[Integer]] -> Integer
checksumA = sum . map minmaxDiff
  where
    minmaxDiff = (-) <$> maximum <*> minimum

checksumB :: [[Integer]] -> Integer
checksumB = sum . map evenDiv
  where
    evenDiv [] = 0
    evenDiv (x:xs) =
        if hasMultiple x xs
            then getResult x xs
            else evenDiv xs
    hasMultiple n = any $ test n
    test a b =
        let (m, n) = sort a b
        in m `mod` n == 0
    sort a b =
        if a >= b
            then (a, b)
            else (b, a)
    getResult n ns =
        let rs = dropWhile (not . test n) ns
            (p, q) = sort n $ head rs
        in p `div` q

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ checksumA input
    print $ checksumB input
