{-# LANGUAGE RecordWildCards #-}

import           Data.Functor
import           Data.List
import           Text.Parsec

loadInput :: IO String
loadInput = readFile "inputs/day-9.txt"

data Thing
    = Garbage String
    | SubGroup Group
    deriving (Show)

newtype Group = Group
    { gThings :: [Thing]
    } deriving (Show)

garbage :: Parsec String () Thing
garbage = char '<' *> (Garbage <$> garbageContent) <* char '>'

garbageContent :: Parsec String () String
garbageContent = intercalate [] <$> many garbagePiece

garbagePiece :: Parsec String () String
garbagePiece = cancelled <|> many1 (noneOf "!>")
  where
    cancelled = char '!' *> anyChar $> ""

thing :: Parsec String () Thing
thing = try (SubGroup <$> grp) <|> garbage <?> "thing"

grp :: Parsec String () Group
grp = Group <$> (char '{' *> many (thing <* optional (char ',')) <* char '}')

parseInput :: String -> Group
parseInput input =
    case result of
        Left e  -> error $ show e
        Right r -> r
  where
    result = parse grp "" input

totalScore :: Group -> Integer
totalScore = scoreFrom 1
  where
    scoreFrom s Group {..} =
        s + (sum . map (scoreFrom (s + 1)) $ [g | SubGroup g <- gThings])

totalGarbage :: Group -> Integer
totalGarbage = sum . map garbageSize . gThings
  where
    garbageSize t =
        case t of
            Garbage s  -> fromIntegral . length $ s
            SubGroup g -> totalGarbage g

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ totalScore input
    print $ totalGarbage input
