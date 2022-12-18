module Day4.Puzzle1 where

import AOC.Prelude

import Data.Array as Array
import Data.String as String
import Node.Encoding as Encoding
import Node.FS.Aff as FS

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day4.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") allText

