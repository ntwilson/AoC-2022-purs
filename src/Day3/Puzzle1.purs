module Day3.Puzzle1 where

import AOC.Prelude

import Data.Array as Array
import Data.Char (toCharCode)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as CUString
import Node.Encoding as Encoding
import Node.FS.Aff as FS

priority :: Char -> Int
priority c | String.toLower (CUString.singleton c) == CUString.singleton c = toCharCode c - 96
priority c | otherwise = toCharCode c - 64 + 26

duplicateItems :: String -> Set Char
duplicateItems bag = Set.intersection firstHalf secondHalf
  where
  firstHalf = String.take compartmentSize bag # CUString.toCharArray # Set.fromFoldable
  secondHalf = String.drop compartmentSize bag # CUString.toCharArray # Set.fromFoldable
  compartmentSize = String.length bag / 2

rucksackScore :: ∀ m. MonadThrow String m => String -> m Int
rucksackScore bag = 
  case duplicateItems bag # Array.fromFoldable of
    [dupeChar] -> pure $ priority dupeChar
    [] -> throwError $ i"Unable to find a duplicate character in bag: '"bag"'"
    dupes -> throwError $ i"Found multiple duplicates: "(show dupes)" in bag: '"bag"'"

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day3.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") allText

ans :: ∀ m. MonadThrow String m => Array String -> m Int
ans input = traverse rucksackScore input <#> sum

run :: ExceptT String Aff Unit
run = getInput >>= ans >>= logShow

