module Day2.Puzzle1 where

import AOC.Prelude

import Data.Array as Array
import Data.String as String
import Node.Encoding as Encoding
import Node.FS.Aff as FS

data Shape = Rock | Paper | Scissors

decodeFromOpponent :: ∀ m. MonadThrow String m => String -> m Shape
decodeFromOpponent "A" = pure Rock
decodeFromOpponent "B" = pure Paper
decodeFromOpponent "C" = pure Scissors
decodeFromOpponent str = throwError $ i"Unable to parse "str" as a Rock, Paper, or Scissors. Expecting 'A', 'B', or 'C'"

decodeFromSelf :: ∀ m. MonadThrow String m => String -> m Shape
decodeFromSelf "X" = pure Rock
decodeFromSelf "Y" = pure Paper
decodeFromSelf "Z" = pure Scissors
decodeFromSelf str = throwError $ i"Unable to parse "str" as a Rock, Paper, or Scissors. Expecting 'X', 'Y', or 'Z'"

scoreShape :: Shape -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

data Outcome = Lose | Draw | Win

scoreOutcome :: Outcome -> Int
scoreOutcome Win = 6
scoreOutcome Lose = 0
scoreOutcome Draw = 3

type Round = { opponentShape :: Shape, selfShape :: Shape, outcome :: Outcome }

scoreRound :: Round -> Int
scoreRound { selfShape, outcome } = scoreShape selfShape + scoreOutcome outcome

evaluateRound :: Shape -> Shape -> Round
evaluateRound opponentShape selfShape = { selfShape, opponentShape, outcome }
  where 
  outcome = case selfShape, opponentShape of
    Rock, Paper -> Lose
    Rock, Scissors -> Win
    Rock, Rock -> Draw

    Paper, Scissors -> Lose
    Paper, Rock -> Win
    Paper, Paper -> Draw

    Scissors, Rock -> Lose
    Scissors, Paper -> Win
    Scissors, Scissors -> Draw

decodeRound :: ∀ m.  Apply m => MonadThrow String m => 
  String -> String -> m Round
decodeRound opponentCode selfCode = 
  evaluateRound <$> decodeFromOpponent opponentCode <*> decodeFromSelf selfCode

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
