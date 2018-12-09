{-# LANGUAGE RecordWildCards #-}

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Text.Parsec         (Parsec, char, digit, letter, many, many1,
                                      newline, option, optional, parse, space,
                                      string, try, (<?>), (<|>))

loadInput :: IO String
loadInput = readFile "inputs/day-8.txt"

data Comparator
    = Equal
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreatThan
    | GreatThanOrEqual
    deriving (Show)

data Operator
    = Increment
    | Decrement
    deriving (Show)

data Condition = Condition
    { cndRegister   :: String
    , cndComparator :: Comparator
    , cndValue      :: Integer
    } deriving (Show)

data Command = Command
    { cmdRegister :: String
    , cmdOperator :: Operator
    , cmdValue    :: Integer
    } deriving (Show)

data Statement = Statement
    { stmCommand   :: Command
    , stmCondition :: Condition
    } deriving (Show)

-- Parsing stuff
label :: Parsec String () String
label = many1 letter

number :: Parsec String () Integer
number = read <$> rawNumber
  where
    rawNumber = (:) <$> option ' ' (char '-') <*> many1 digit

comparator :: Parsec String () Comparator
comparator =
    try (Equal <$ string "==") <|> try (NotEqual <$ string "!=") <|>
    try (LessThanOrEqual <$ string "<=") <|>
    try (LessThan <$ string "<") <|>
    try (GreatThanOrEqual <$ string ">=") <|>
    (GreatThan <$ string ">") <?> "comparator"

condition :: Parsec String () Condition
condition =
    Condition <$> (string "if " *> label) <*> (space *> comparator) <*>
    (space *> number)

operator :: Parsec String () Operator
operator =
    try (Increment <$ string "inc") <|>
    (Decrement <$ string "dec") <?> "operator"

command :: Parsec String () Command
command = Command <$> label <*> (space *> operator) <*> (space *> number)

statement :: Parsec String () Statement
statement = Statement <$> command <*> (space *> condition) <* optional newline

parseInput :: String -> [Statement]
parseInput input =
    case result of
        Left e  -> error $ show e
        Right r -> r
  where
    result = parse (many statement) "" input

data CPU = CPU
    { cpuRegistries      :: M.Map String Integer
    , cpuLargestRegistry :: Integer
    } deriving (Show)

-- Actual problem
evalCondition :: Condition -> State CPU Bool
-- cndRegister cndValue
evalCondition Condition {..} = do
    registerValue <- gets (fromMaybe 0 . M.lookup cndRegister . cpuRegistries)
    return $
        case cndComparator of
            Equal            -> registerValue == cndValue
            NotEqual         -> registerValue /= cndValue
            LessThan         -> registerValue < cndValue
            LessThanOrEqual  -> registerValue <= cndValue
            GreatThan        -> registerValue > cndValue
            GreatThanOrEqual -> registerValue >= cndValue

execCommand :: Command -> State CPU ()
execCommand Command {..} =
    case cmdOperator of
        Increment ->
            modify
                (\c@CPU {..} ->
                     c
                     { cpuRegistries =
                           M.insertWith (+) cmdRegister cmdValue cpuRegistries
                     })
        Decrement ->
            modify
                (\c@CPU {..} ->
                     c
                     { cpuRegistries =
                           M.insertWith
                               (+)
                               cmdRegister
                               (negate cmdValue)
                               cpuRegistries
                     })

updateLatestRegistry :: State CPU ()
updateLatestRegistry =
    modify
        (\c@CPU {..} ->
             c
             { cpuLargestRegistry =
                   maximum (cpuLargestRegistry : M.elems cpuRegistries)
             })

execStatement :: Statement -> State CPU ()
execStatement Statement {..} = do
    condition <- evalCondition stmCondition
    when condition $ execCommand stmCommand
    updateLatestRegistry

finalCPU :: [Statement] -> CPU
finalCPU input = execState (forM input execStatement) (CPU M.empty 0)

currentLargestRegistry :: [Statement] -> Integer
currentLargestRegistry = maximum . M.elems . cpuRegistries . finalCPU

largestRegistry :: [Statement] -> Integer
largestRegistry = cpuLargestRegistry . finalCPU

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ currentLargestRegistry input
    print $ largestRegistry input
