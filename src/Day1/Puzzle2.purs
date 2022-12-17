module Day1.Puzzle2 where

import AOC.Prelude

import Data.Array as Array
import Day1.Puzzle1 (getInput, parseInput)

ans :: Array String -> Int
ans input = sum $ Array.take 3 caloriesPerElf
  where
  caloriesPerElf = Array.sortWith negate $ (sum <$> parseInput input)

run :: ExceptT String Aff Unit 
run = logShow =<< (ans <$> getInput)


