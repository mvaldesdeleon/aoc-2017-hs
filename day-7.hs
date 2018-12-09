{-# LANGUAGE RecordWildCards #-}

import           Data.Function
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set        as S
import           Text.Parsec     (Parsec, digit, letter, many1, newline,
                                  optional, parse, string, (<|>))

loadInput :: IO String
loadInput = readFile "inputs/day-7.txt"

data ProgramRaw = ProgramRaw
    { prName   :: String
    , prWeight :: Integer
    , prDisc   :: [String]
    } deriving (Show)

label :: Parsec String () String
label = many1 letter

integer :: Parsec String () Integer
integer = read <$> many1 digit

disc :: Parsec String () [String]
disc = string " -> " *> many1 (label <* optional (string ", "))

programRaw :: Parsec String () ProgramRaw
programRaw =
    ProgramRaw <$> label <*> (string " (" *> integer <* string ")") <*>
    (disc <|> pure []) <*
    optional newline

parseInput :: String -> [ProgramRaw]
parseInput input =
    case result of
        Left e  -> error $ show e
        Right r -> r
  where
    result = parse (many1 programRaw) "" input

root :: [ProgramRaw] -> String
root ps = S.elemAt 0 (all S.\\ discs)
  where
    (all, discs) = foldl f (S.empty, S.empty) ps
    f (all, discs) ProgramRaw {..} =
        (S.insert prName all, S.union (S.fromList prDisc) discs)

data ProgramTree = ProgramTree
    { ptName       :: String
    , ptWeight     :: Integer
    , ptTreeWeight :: Integer
    , ptDisc       :: [ProgramTree]
    } deriving (Show)

programTree :: [ProgramRaw] -> ProgramTree
programTree ps = buildTree pMap rootName
  where
    pMap = foldl f M.empty ps
    f pMap p = M.insert (prName p) p pMap
    rootName = root ps

buildTree :: M.Map String ProgramRaw -> String -> ProgramTree
buildTree pMap root =
    ProgramTree
        prName
        prWeight
        (prWeight + (sum . map ptTreeWeight $ subTrees))
        subTrees
  where
    ProgramRaw {..} = pMap M.! root
    subTrees = map (buildTree pMap) prDisc

isBalanced :: ProgramTree -> Bool
isBalanced = allEqual . map ptTreeWeight . ptDisc

allEqual :: Eq a => [a] -> Bool
allEqual (x:xs) = all (== x) xs
allEqual []     = True

balanceTree :: ProgramTree -> Maybe Integer
balanceTree tree =
    if isBalanced tree
        then Nothing
        else case find (not . isBalanced) (ptDisc tree) of
                 Just t  -> balanceTree t
                 Nothing -> Just . fixedWeight . ptDisc $ tree

fixedWeight :: [ProgramTree] -> Integer
fixedWeight ts = twr - tww + ww
  where
    groups =
        groupBy ((==) `on` ptTreeWeight) . sortBy (compare `on` ptTreeWeight) $
        ts
    (ProgramTree _ wr twr _) = head . head . filter ((> 1) . length) $ groups
    (ProgramTree _ ww tww _) = head . head . filter ((== 1) . length) $ groups

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ root input
    print $ fromMaybe 0 (balanceTree . programTree $ input)
