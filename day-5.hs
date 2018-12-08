-- Day 5
import           Control.Monad.State
import           Data.Map            (Map, adjust, fromList, size, (!))

loadInput :: IO String
loadInput = readFile "inputs/day-5.txt"

parseInput :: String -> Map Integer Integer
parseInput = fromList . zip [0 ..] . map (toInteger . read) . lines

data Program = Program
    { jumps    :: Map Integer Integer
    , position :: Integer
    , count    :: Integer
    } deriving (Show)

valid :: Program -> Bool
valid prg =
    let pos = position prg
    in (pos >= 0) && (pos < (fromIntegral . size . jumps $ prg))

step :: Bool -> State Program Bool
step partB = do
    modify advance
    gets valid
  where
    advance (Program jumps position count) =
        Program
            (adjust update (fromIntegral position) jumps)
            (position + (jumps ! fromIntegral position))
            (count + 1)
    update offset =
        if partB && offset >= 3
            then offset - 1
            else offset + 1

eval :: Bool -> State Program Integer
eval partB = do
    valid <- step partB
    if valid
        then eval partB
        else gets count

countJumps :: Bool -> Map Integer Integer -> Integer
countJumps partB input = evalState (eval partB) (Program input 0 0)

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ countJumps False input
    print $ countJumps True input
