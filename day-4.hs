import           Data.List

loadInput :: IO String
loadInput = readFile "inputs/day-4.txt"

parseInput :: String -> [[String]]
parseInput = map words . lines

countValidA :: [[String]] -> Integer
countValidA = toInteger . length . filter valid
  where
    valid xs = length xs == length (nub xs)

countValidB :: [[String]] -> Integer
countValidB = toInteger . length . filter valid
  where
    valid xs = noDupes xs && noAnas xs
    noDupes xs = length xs == length (nub xs)
    noAnas = noDupes . map sort

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ countValidA input
    print $ countValidB input
