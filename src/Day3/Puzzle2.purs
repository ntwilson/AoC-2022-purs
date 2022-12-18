module Day3.Puzzle2 where

import AOC.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as CUString
import Data.Tuple (Tuple(..), fst, snd)
import Node.Encoding as Encoding
import Node.FS.Aff as FS

priority :: Char -> Int
priority c | String.toLower (CUString.singleton c) == CUString.singleton c = toCharCode c - 96
priority c | otherwise = toCharCode c - 64 + 26

chunkBySize :: ∀ a. Int -> Array a -> Array (NonEmptyArray a)
chunkBySize size xs = 
  Array.mapWithIndex Tuple xs
  # Array.groupBy (eq `on` (fst >>> (_ / size)))
  # map (map snd)

groupBadges :: Array String -> Array (Set Char)
groupBadges inputs =
  inputs 
  <#> (CUString.toCharArray >>> Set.fromFoldable)
  # chunkBySize 3
  <#> allIntersections

  where
  allIntersections xs = foldl (Set.intersection) (NonEmptyArray.head xs) (NonEmptyArray.tail xs)

groupScore :: forall m. Applicative m => MonadThrow String m => Set Char -> m Int
groupScore xs = case Array.fromFoldable xs of 
  [badge] -> pure $ priority badge
  [] -> throwError "Unable to find a badge."
  dupes -> throwError $ i"Found multiple duplicates: "(show dupes)

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day3.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") allText

ans :: ∀ m. MonadThrow String m => Array String -> m Int
ans input = do
  scores <- traverse groupScore $ groupBadges input
  pure $ sum scores

testInput :: Array String
testInput = 
  [ "vJrwpWtwJgWrhcsFMMfFFhFp"
  , "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
  , "PmmdzqPrVvPwwTWBwg"
  , "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
  , "ttgJtRGJQctTZtZT"
  , "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]

run :: ExceptT String Aff Unit
run = getInput >>= ans >>= logShow
