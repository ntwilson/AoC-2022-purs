module Day6.Puzzle1 where

import AOC.Prelude

import Data.Array as Array
import Data.Set as Set
import Data.String.CodeUnits as CUString
import Data.Tuple (fst, snd)
import Node.Encoding as Encoding
import Node.FS.Aff as FS

getInput :: ExceptT String Aff String
getInput = withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day6.txt"

testCases :: Array { input :: String, solution :: Int }
testCases = 
  [ { input: "mjqjpqmgbljsphdztnvjfqwrcgsmlb", solution: 7 }
  , { input: "bvwbjplbgvbhsrlpgdmjqwftvncz", solution: 5 }
  , { input: "nppdvjthqldpwncqszvftbrmjlhg", solution: 6 }
  , { input: "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", solution: 10 }
  , { input: "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", solution: 11 }
  ]

window :: ∀ a. Int -> Array a -> Array (Array a)
window size xs 
  | size <= Array.length xs = 
    (0 .. (Array.length xs - size)) 
    <#> \i -> Array.slice i (i+size) xs
  | otherwise = [xs]


ans :: String -> Maybe Int
ans input = 
  input 
  # CUString.toCharArray
  # Array.mapWithIndex Tuple
  # window 4
  # Array.find (\tokens -> (Set.fromFoldable (snd <$> tokens) # Set.size) == 4)
  >>= Array.last
  <#> (fst >>> (_+1))

run :: ExceptT String Aff Unit
run = getInput >>= (ans >>> note "Unable to find a start-of-message token" >>> liftEither) >>= logShow

