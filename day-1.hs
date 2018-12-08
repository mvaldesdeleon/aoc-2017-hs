-- Day 1
import           Control.Applicative
import           Data.Char           (digitToInt)

loadInput :: IO String
loadInput = readFile "inputs/day-1.txt"

captchaA :: String -> Integer
captchaA = generalCaptcha 1

captchaB :: String -> Integer
captchaB = generalCaptcha <$> toInteger . rot <*> id
  where
    rot s = length s `div` 2

generalCaptcha :: Integer -> String -> Integer
generalCaptcha n input = sum $ map (toInteger . digitToInt) valid
  where
    valid = zipWith keepDupes (rotate n input) input
    keepDupes a b =
        if a == b
            then a
            else '0'

rotate :: Integer -> [a] -> [a]
rotate n as = zipWith const (drop (fromInteger n) $ cycle as) as

main :: IO ()
main = do
    input <- loadInput
    print $ captchaA input
    print $ captchaB input
