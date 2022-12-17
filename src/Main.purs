module Main where

import AOC.Prelude

import Day1.Puzzle1 as Day1.Puzzle1
import Day1.Puzzle2 as Day1.Puzzle2

run :: ExceptT String Aff Unit
run = do
  log "Day1.Puzzle1:"
  Day1.Puzzle1.run
  log "Day1.Puzzle2:"
  Day1.Puzzle2.run

main :: Effect Unit
main = launchAff_ do
  maybeResult <- runExceptT run
  case maybeResult of 
    Left err -> log err
    Right unit -> pure unit
