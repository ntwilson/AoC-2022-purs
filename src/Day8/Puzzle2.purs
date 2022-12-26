module Day8.Puzzle2 where

import AOC.Prelude

import Data.Array as Array
import Data.Int as Int
import Data.String as String
import Data.String.CodeUnits as CUString
import Data.TraversableWithIndex (traverseWithIndex)
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Parsing (parseErrorMessage)
import Parsing as Parsing
import Parsing.String.Basic as Parsing

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day8.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") allText

testInput :: Array String 
testInput = 
  [ "30373"
  , "25512"
  , "65332"
  , "33549"
  , "35390"
  ]

parseLine :: ∀ m. MonadThrow String m => String -> m (Array Int)
parseLine s = liftEither $ lmap parseErrorMessage $ runParser s $ Array.many do
  digit <- Parsing.digit 
  maybe (Parsing.fail "Expected a number") pure $ Int.fromString $ CUString.singleton digit

parseInput :: forall m. Applicative m => MonadThrow String m => Array String -> m (Array (Array Int))
parseInput = traverse parseLine

scenicScore :: ∀ m. MonadThrow String m => Int -> Int -> Array (Array Int) -> m Int
scenicScore row column input = do
  targetTree <- input # index row >>= index column # noteM (i"Unable to find tree at index ("row","column")")
  pure $ (visibleLeft * visibleRight * visibleUp * visibleDown) targetTree row column 

  where
  visibleLeft = visibleAt (_ - 1) identity 
  visibleRight = visibleAt (_ + 1) identity
  visibleUp = visibleAt identity (_ - 1)
  visibleDown = visibleAt identity (_ + 1)

  index :: ∀ a. Int -> Array a -> Maybe a
  index = flip Array.index 

  visibleAt modifyRow modifyColumn targetTree = go
    where
    go :: Int -> Int -> Int
    go r c = case input # index (modifyRow r) >>= index (modifyColumn c) of
      Just adjacent | (adjacent < targetTree) -> 1 + go (modifyRow r) (modifyColumn c)
      Just _ | otherwise -> 1
      Nothing -> 0


solution :: forall m. MonadThrow String m => Array (Array Int) -> m Int
solution input = do
  grid <- scoreGrid input
  maxByRow <- traverse maxM grid
  maxM maxByRow

  where
  scoreGrid input = input # traverseWithIndex \r row -> row # traverseWithIndex \c _col -> scenicScore r c input
  maxM = maximum >>> noteM "Encountered an empty row or column in the input"

ans :: forall m. MonadThrow String m => Array String -> m Int
ans = parseInput >=> solution 

run :: ExceptT String Aff Unit
run = getInput >>= ans >>= logShow
