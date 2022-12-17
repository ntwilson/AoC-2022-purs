module Day1.Puzzle1 where

import AOC.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Int as Int
import Data.String as String
import Node.Encoding as Encoding
import Node.FS.Aff as FS

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day1Puzzle1.txt"
  pure $ String.split (Pattern "\n") allText


parseInput :: Array String -> Array (Array Int)
parseInput input = 
  input
  # Array.groupBy (eq `on` String.null)
  # Array.filter (_ /= NonEmptyArray.singleton "")
  # map (NonEmptyArray.toArray >>> filterMap Int.fromString)

ans :: Array String -> Int
ans input = fromMaybe 0 $ maximum $ (sum <$> parseInput input)

run :: ExceptT String Aff Unit
run = logShow =<< (ans <$> getInput)

