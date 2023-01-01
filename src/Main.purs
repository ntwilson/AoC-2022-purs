module Main where

import AOC.Prelude

import Day1.Puzzle1 as Day1.Puzzle1
import Day1.Puzzle2 as Day1.Puzzle2
import Day10.Puzzle1 as Day10.Puzzle1
import Day10.Puzzle2 as Day10.Puzzle2
import Day11.Puzzle1 as Day11.Puzzle1
import Day11.Puzzle2 as Day11.Puzzle2
import Day12.Puzzle1 as Day12.Puzzle1
import Day12.Puzzle2 as Day12.Puzzle2
import Day13.Puzzle1 as Day13.Puzzle1
import Day13.Puzzle2 as Day13.Puzzle2
import Day14.Puzzle1 as Day14.Puzzle1
import Day14.Puzzle2 as Day14.Puzzle2
import Day15.Puzzle1 as Day15.Puzzle1
import Day2.Puzzle1 as Day2.Puzzle1
import Day2.Puzzle2 as Day2.Puzzle2
import Day3.Puzzle1 as Day3.Puzzle1
import Day3.Puzzle2 as Day3.Puzzle2
import Day4.Puzzle1 as Day4.Puzzle1
import Day4.Puzzle2 as Day4.Puzzle2
import Day5.Puzzle1 as Day5.Puzzle1
import Day5.Puzzle2 as Day5.Puzzle2
import Day6.Puzzle1 as Day6.Puzzle1
import Day6.Puzzle2 as Day6.Puzzle2
import Day7.Puzzle1 as Day7.Puzzle1
import Day7.Puzzle2 as Day7.Puzzle2
import Day8.Puzzle1 as Day8.Puzzle1
import Day8.Puzzle2 as Day8.Puzzle2
import Day9.Puzzle1 as Day9.Puzzle1
import Day9.Puzzle2 as Day9.Puzzle2

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
  log "Day5.Puzzle1:"
  Day5.Puzzle1.run
  log "Day5.Puzzle2:"
  Day5.Puzzle2.run
  log "Day6.Puzzle1:"
  Day6.Puzzle1.run
  log "Day6.Puzzle2:"
  Day6.Puzzle2.run
  log "Day7.Puzzle1:"
  Day7.Puzzle1.run
  log "Day7.Puzzle2:"
  Day7.Puzzle2.run
  log "Day8.Puzzle1:"
  Day8.Puzzle1.run
  log "Day8.Puzzle2:"
  Day8.Puzzle2.run
  log "Day9.Puzzle1:"
  Day9.Puzzle1.run
  log "Day9.Puzzle2:"
  Day9.Puzzle2.run
  log "Day10.Puzzle1:"
  Day10.Puzzle1.run
  log "Day10.Puzzle2:"
  Day10.Puzzle2.run
  log "Day11.Puzzle1:"
  Day11.Puzzle1.run
  log "Day11.Puzzle2:"
  Day11.Puzzle2.run
  log "Day12.Puzzle1:"
  Day12.Puzzle1.run
  log "Day12.Puzzle2:"
  Day12.Puzzle2.run
  log "Day13.Puzzle1:"
  Day13.Puzzle1.run
  log "Day13.Puzzle2:"
  Day13.Puzzle2.run
  log "-- Day 14 takes a couple minutes to run, and so is being skipped."
  log "-- Edit Main.purs to run `printDay14Solution` to access the solution."
  log "-- (and zoom your terminal window out significantly for Puzzle 2, since the solution takes a lot of screen space)"
  log "Day15.Puzzle1:"
  Day15.Puzzle1.run


printDay14EmptyMap :: ExceptT String Aff Unit
printDay14EmptyMap = do 
  input <- Day14.Puzzle1.getInput 
  grid <- Day14.Puzzle1.parseGrid input
  Day14.Puzzle1.printGrid grid

printDay14Solution :: ExceptT String Aff Unit
printDay14Solution = do 
  input <- Day14.Puzzle2.getInput 
  grid <- Day14.Puzzle2.parseGrid input
  let ans = Day14.Puzzle2.dropAsMuchSandAsPossible grid
  Day14.Puzzle2.printGrid ans
  logShow $ Day14.Puzzle2.solution ans

main :: Effect Unit
main = launchAff_ do
  maybeResult <- runExceptT run
  case maybeResult of 
    Left err -> log err
    Right unit -> pure unit
