-- Day 3

input :: Integer
input = 361527

-- Identifies to which even-sided layer a position belongs
layer :: Integer -> Integer
layer input = toInteger $ length $ takeWhile (< input) layers
    where layers = [(2*x+1)^2 | x <- [0..]]

-- Returns the first position for an even-sided layer
base :: Integer -> Integer
base layer = if layer <= 0 then 0
                           else (2*(layer - 1)+1)^2 + 1

steps :: Integer -> Integer
steps input = if ly == 0 then 0
                         else ly + (abs $ cornerOffset input - (ly - 1))
    where ly = layer input
          -- Returns the corner offset for a given position
          cornerOffset input = (input - base ly) `mod` (ly * 2)

main :: IO ()
main = print $ steps input
