module Test.Main where

import AOC.Prelude

import Data.Foldable (for_)
import Day6.Puzzle1 as Day6.Puzzle1
import Day6.Puzzle2 as Day6.Puzzle2
import Day7.Puzzle1 (Input(..))
import Day7.Puzzle1 as Day7.Puzzle1
import Day7.Puzzle2 as Day7.Puzzle2
import Day8.Puzzle1 as Day8.Puzzle1
import Day9.Puzzle1 as Day9.Puzzle1
import Day9.Puzzle2 as Day9.Puzzle2
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
      it "successfully parses the test input" do
        Day7.Puzzle1.parseAllInput Day7.Puzzle1.testInput `shouldEqual`
          Right
            [ FileSize 14848514
            , FileSize 8504156
            , NavigateIn "a"
            , FileSize 29116
            , FileSize 2557
            , FileSize 62596
            , NavigateIn "e"
            , FileSize 584
            , NavigateOut
            , NavigateOut
            , NavigateIn "d"
            , FileSize 4060174
            , FileSize 8033020
            , FileSize 5626152
            , FileSize 7214296
            ] 
          
      it "passes the provided test case" do
        Day7.Puzzle1.ans Day7.Puzzle1.testInput `shouldEqual` Right 95437 -- (dir a + e)

    describe "Day 7 Puzzle 2" do
      it "passes the provided test case" do
        Day7.Puzzle2.ans Day7.Puzzle2.testInput `shouldEqual` Right 24933642 -- dir d

    describe "Day 8 Puzzle 1" do
      it "passes the provided test case" do
        Day8.Puzzle1.ans Day8.Puzzle1.testInput `shouldEqual` Right 21

    describe "Day 9 Puzzle 1" do
      it "passes the provided test case" do
        Day9.Puzzle1.ans Day9.Puzzle1.testInput `shouldEqual` Right 13

    describe "Day 9 Puzzle 2" do
      it "passes the provided test case" do
        Day9.Puzzle2.ans Day9.Puzzle2.testInput `shouldEqual` Right 36
