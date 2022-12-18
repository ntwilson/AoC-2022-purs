module Main where

import AOC.Prelude

import Day1.Puzzle1 as Day1.Puzzle1
import Day1.Puzzle2 as Day1.Puzzle2
import Day2.Puzzle1 as Day2.Puzzle1
import Day2.Puzzle2 as Day2.Puzzle2
import Day3.Puzzle1 as Day3.Puzzle1
import Day3.Puzzle2 as Day3.Puzzle2
import Day4.Puzzle1 as Day4.Puzzle1
import Day4.Puzzle2 as Day4.Puzzle2

run :: ExceptT String Aff Unit
run = do
  log "Day1.Puzzle1:"
  Day1.Puzzle1.run
  log "Day1.Puzzle2:"
  Day1.Puzzle2.run
  log "Day2.Puzzle1:"
  Day2.Puzzle1.run
  log "Day2.Puzzle2:"
  Day2.Puzzle2.run
  log "Day3.Puzzle1:"
  Day3.Puzzle1.run
  log "Day3.Puzzle2:"
  Day3.Puzzle2.run
  log "Day4.Puzzle1:"
  Day4.Puzzle1.run
  log "Day4.Puzzle2:"
  Day4.Puzzle2.run

main :: Effect Unit
main = launchAff_ do
  maybeResult <- runExceptT run
  case maybeResult of 
    Left err -> log err
    Right unit -> pure unit
