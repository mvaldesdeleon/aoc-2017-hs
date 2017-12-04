-- Day 3

import Data.Map

type Coords = (Integer, Integer)

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
steps input = ly + abs (centerOffset input)
    where ly = layer input

-- Returns the corner offset for a given position
centerOffset :: Integer -> Integer
centerOffset input = if input <= 1 then 0
                                   else cornerOffset - (ly - 1)
    where ly = layer input
          cornerOffset = (input - base ly) `mod` (ly * 2)

coords :: Integer -> Coords
coords input = if input <= 1 then (0, 0)
                             else makeCoords (side input) (centerOffset input)
    where ly = layer input
          side input = (input - base ly) `div` (ly * 2)
          makeCoords 0 offset = (ly, offset)
          makeCoords 1 offset = (-offset, ly)
          makeCoords 2 offset = (-ly, -offset)
          makeCoords 3 offset = (offset, -ly)

-- given the state (map) and a position (n), we can generate the updated state, the next position, and the last value

first :: Integer -> Integer
first input = let map = singleton (0,0) 1
                  n = 2
                  in next map n
    where next map n = let c = coords n
                           v = neighbors map c
                           in if v > input then v
                                           else next (insert c v map) (n + 1)

neighbors :: Map Coords Integer -> Coords -> Integer
neighbors map (x, y) = get (x + 1, y) + get (x + 1, y + 1) +
                       get (x, y + 1) + get (x - 1, y + 1) +
                       get (x - 1, y) + get (x - 1, y - 1) +
                       get (x, y - 1) + get (x + 1, y - 1)
    where get c = maybe 0 id $ Data.Map.lookup c map

main :: IO ()
main = print (steps input) >> print (first input)
