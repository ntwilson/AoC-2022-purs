module Day2.Puzzle2 where

import AOC.Prelude

import Data.Array as Array
import Data.String as String
import Node.Encoding as Encoding
import Node.FS.Aff as FS

data Shape = Rock | Paper | Scissors

decodeShape :: ∀ m. MonadThrow String m => String -> m Shape
decodeShape "A" = pure Rock
decodeShape "B" = pure Paper
decodeShape "C" = pure Scissors
decodeShape str = throwError $ i"Unable to parse "str" as a Rock, Paper, or Scissors. Expecting 'A', 'B', or 'C'"

scoreShape :: Shape -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

data Outcome = Lose | Draw | Win

decodeOutcome :: ∀ m. MonadThrow String m => String -> m Outcome
decodeOutcome "X" = pure Lose
decodeOutcome "Y" = pure Draw
decodeOutcome "Z" = pure Win
decodeOutcome str = throwError $ i"Unable to parse "str" as an outcome. Expecting 'X', 'Y', or 'Z'"

scoreOutcome :: Outcome -> Int
scoreOutcome Lose = 0
scoreOutcome Draw = 3
scoreOutcome Win = 6

type Round = { opponentShape :: Shape, selfShape :: Shape, outcome :: Outcome }

scoreRound :: Round -> Int
scoreRound { selfShape, outcome } = scoreShape selfShape + scoreOutcome outcome

evaluateRound :: Shape -> Outcome -> Round
evaluateRound opponentShape outcome = { selfShape, opponentShape, outcome }
  where 
  selfShape = case opponentShape, outcome of
    Rock, Lose -> Scissors
    Rock, Draw -> Rock
    Rock, Win -> Paper

    Paper, Lose -> Rock
    Paper, Draw -> Paper
    Paper, Win -> Scissors

    Scissors, Lose -> Paper
    Scissors, Draw -> Scissors
    Scissors, Win -> Rock

decodeRound :: ∀ m.  Apply m => MonadThrow String m => 
  String -> String -> m Round
decodeRound opponentCode selfCode = 
  evaluateRound <$> decodeShape opponentCode <*> decodeOutcome selfCode

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day2.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") allText

parseInput :: ∀ m. Apply m => MonadThrow String m => Array String -> m (Array Round)
parseInput input = traverse parseLine input
  where
  parseLine line = case String.split (Pattern " ") line of
    [opponent, self] -> decodeRound opponent self
    _ -> throwError $ i"Expecting two inputs per line, got: "line

ans :: ExceptT String Aff Int
ans = do
  rounds <- parseInput =<< getInput
  pure $ sum (scoreRound <$> rounds)

run :: ExceptT String Aff Unit
run = logShow =<< ans
