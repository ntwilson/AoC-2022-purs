module Test.Main where

import AOC.Prelude

import Data.Foldable (for_)
import Day6.Puzzle1 as Day6.Puzzle1
import Day6.Puzzle2 as Day6.Puzzle2
import Day7.Puzzle1 as Day7.Puzzle1
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ do
  runSpec [consoleReporter] do
    describe "Day 6 Puzzle 1" do
      for_ Day6.Puzzle1.testCases \{ input, solution } -> do
        it (i"passes test case '"input"'") do
          Day6.Puzzle1.ans input `shouldEqual` Just solution

    describe "Day 6 Puzzle 2" do
      for_ Day6.Puzzle2.testCases \{ input, solution } -> do
        it (i"passes test case '"input"'") do
          Day6.Puzzle2.ans input `shouldEqual` Just solution

    describe "Day 7 Puzzle 1" do
      it "passes the provided test case" do
        Day7.Puzzle1.ans Day7.Puzzle1.testInput `shouldEqual` 95437
