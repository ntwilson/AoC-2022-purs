module Day10.Puzzle2 where

import AOC.Prelude

import Data.Array as Array
import Data.String as String
import Data.String.CodeUnits as CU.String
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Parsing (parseErrorMessage)
import Parsing.Combinators as Parsing
import Parsing.String as Parsing
import Parsing.String.Basic as Parsing

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day10.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") allText

testInput :: Array String
testInput = 
  [ "addx 15", "addx -11", "addx 6", "addx -3", "addx 5", "addx -1", "addx -8", "addx 13", "addx 4", "noop"
  , "addx -1", "addx 5", "addx -1", "addx 5", "addx -1", "addx 5", "addx -1", "addx 5", "addx -1", "addx -35"
  , "addx 1", "addx 24", "addx -19", "addx 1", "addx 16", "addx -11", "noop", "noop", "addx 21", "addx -15"
  , "noop", "noop", "addx -3", "addx 9", "addx 1", "addx -3", "addx 8", "addx 1", "addx 5", "noop", "noop"
  , "noop", "noop", "noop", "addx -36", "noop", "addx 1", "addx 7", "noop", "noop", "noop", "addx 2", "addx 6"
  , "noop", "noop", "noop", "noop", "noop", "addx 1", "noop", "noop", "addx 7", "addx 1", "noop", "addx -13"
  , "addx 13", "addx 7", "noop", "addx 1", "addx -33", "noop", "noop", "noop", "addx 2", "noop", "noop"
  , "noop", "addx 8", "noop", "addx -1", "addx 2", "addx 1", "noop", "addx 17", "addx -9", "addx 1", "addx 1"
  , "addx -3", "addx 11", "noop", "noop", "addx 1", "noop", "addx 1", "noop", "noop", "addx -13", "addx -19"
  , "addx 1", "addx 3", "addx 26", "addx -30", "addx 12", "addx -1", "addx 3", "addx 1", "noop", "noop", "noop"
  , "addx -9", "addx 18", "addx 1", "addx 2", "noop", "noop", "addx 9", "noop", "noop", "noop", "addx -1"
  , "addx 2", "addx -37", "addx 1", "addx 3", "noop", "addx 15", "addx -21", "addx 22", "addx -6", "addx 1"
  , "noop", "addx 2", "addx 1", "noop", "addx -10", "noop", "noop", "addx 20", "addx 1", "addx 2", "addx 2"
  , "addx -6", "addx -11", "noop", "noop", "noop"
  ]

data Instruction = AddX Int | Noop

parseInstruction :: ∀ m. MonadThrow String m => String -> m Instruction
parseInstruction s = liftEither $ lmap parseErrorMessage $ runParser s do
  parseAddX <|> parseNoop
  where 
  parseAddX = Parsing.try (Parsing.string "addx " *> Parsing.intDecimal) <#> AddX
  parseNoop = Parsing.try (Parsing.string "noop") $> Noop

parseInput :: ∀ m. MonadThrow String m => Array String -> m (Array Instruction)
parseInput = traverse parseInstruction

chunkBySize :: Int -> String -> Array String
chunkBySize size str 
  | String.length str <= size = [str]
  | otherwise =  
    let { before, after } = String.splitAt size str
    in Array.cons before (chunkBySize size after)

solution :: ∀ m. MonadThrow String m => Array Instruction -> m (Array String)
solution instructions = chunkBySize 40 <<< renderLine <$> pixels

  where
  cpuStates = Array.cons initialState $ scanl fold initialState instructions
  initialState = { register: 1, clock: 0 } 
  fold {register, clock} = case _ of 
    AddX i -> {register: register + i, clock: clock + 2}
    Noop -> {register, clock: clock + 1}

  register targetClock = Array.takeWhile (\{clock} -> clock <= targetClock) cpuStates # Array.last # noteM (i"Couldn't advance clock to value "targetClock) <#> _.register

  renderLine = CU.String.fromCharArray <<< map renderPixel
  renderPixel true = '#'
  renderPixel false = '.'

  pixelLit clock = do
    sprite <- register clock
    pure $ between (sprite - 1) (sprite + 1) (clock `mod` 40)

  pixels = traverse pixelLit (0 .. 239)

ans :: ∀ m. MonadThrow String m => Array String -> m (Array String)
ans = parseInput >=> solution

run :: ExceptT String Aff Unit
run = getInput >>= ans >>= traverse_ log

