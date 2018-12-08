{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

import           Control.Monad.Loops
import           Control.Monad.State
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S

loadInput :: IO String
loadInput = readFile "inputs/day-6.txt"

parseInput :: String -> [Bank]
parseInput = zipWith Bank [0 ..] . map read . words . filter ('\n' /=)

data Bank = Bank
    { index  :: Integer
    , blocks :: Integer
    }

instance Show Bank where
    show (Bank _ blocks) = show blocks

instance Eq Bank where
    (Bank _ a) == (Bank _ b) = a == b

instance Ord Bank where
    (Bank i a) <= (Bank j b) =
        if a == b
            then i >= j
            else a <= b

data Cycle = Cycle
    { count    :: Integer
    , banks    :: [Bank]
    , previous :: M.Map [Bank] Integer
    } deriving (Show)

detectLoop :: State Cycle [[Bank]]
detectLoop = reallocate `untilM` seenBefore
  where
    seenBefore = do
        Cycle {..} <- get
        return $ banks `M.member` previous

reallocate :: State Cycle [Bank]
reallocate = do
    recordCurrent
    (Bank index blocks) <- getNextBank
    clearBank index
    redistribute blocks index
    countCycle
    gets banks
  where
    recordCurrent =
        modify (\cy@Cycle {..} -> cy {previous = M.insert banks count previous})
    getNextBank = gets (maximum . banks)
    clearBank index =
        modify
            (\cy@Cycle {..} -> cy {banks = replace index (Bank index 0) banks})
    countCycle = modify (\cy@Cycle {..} -> cy {count = count + 1})

redistribute :: Integer -> Integer -> State Cycle ()
redistribute blocks index = do
    size <- gets (fromIntegral . length . banks)
    forM_ [1 .. blocks] (allocateBlock . blockIndex size)
  where
    blockIndex size = within size . fromIntegral . (+) index
    within max value = value `mod` max
    allocateBlock to = do
        (Bank _ blocks) <- gets ((!! fromIntegral to) . banks)
        modify
            (\cy@Cycle {..} ->
                 cy {banks = replace to (Bank to (blocks + 1)) banks})

finalCycle :: [Bank] -> Cycle
finalCycle banks = execState detectLoop (Cycle 0 banks M.empty)

cycles :: [Bank] -> Integer
cycles = count . finalCycle

loopSize :: [Bank] -> Integer
loopSize = size . finalCycle
  where
    size Cycle {..} = count - (previous M.! banks)

replace :: Integer -> a -> [a] -> [a]
replace i v (a:as) =
    if i == 0
        then v : as
        else a : replace (i - 1) v as
replace _ _ [] = []

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ cycles input
    print $ loopSize input
