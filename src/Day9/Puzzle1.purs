module Day9.Puzzle1 where

import AOC.Prelude

import Data.Array as Array
import Data.Set as Set
import Data.String as String
import Data.Traversable (scanl)
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Parsing (parseErrorMessage)
import Parsing as Parsing
import Parsing.String.Basic as Parsing

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day9.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") allText

testInput :: Array String 
testInput = 
  [ "R 4"
  , "U 4"
  , "L 3"
  , "D 1"
  , "R 4"
  , "D 1"
  , "L 5"
  , "R 2"
  ]

data Direction = MoveUp | MoveDown | MoveLeft | MoveRight
type Movement = { direction :: Direction, count :: Int }

parseLine :: ∀ m. MonadThrow String m => String -> m Movement
parseLine s = liftEither $ lmap parseErrorMessage $ runParser s do
  direction <- parseDirection <* Parsing.skipSpaces
  count <- Parsing.intDecimal
  pure { direction, count }

  where 
  parseDirection = do
    letter <- Parsing.letter
    case letter of 
      'U' -> pure MoveUp
      'D' -> pure MoveDown
      'L' -> pure MoveLeft
      'R' -> pure MoveRight
      c -> Parsing.fail $ i"Expected direction of U|D|L|R, but got '"(show c)"'"



parseInput :: forall m. Applicative m => MonadThrow String m => Array String -> m (Array Movement)
parseInput = traverse parseLine

type Position = { x :: Int, y :: Int }

chaseHead :: Position -> Position -> Position
chaseHead tail head = 
  if abs (head.x - tail.x) <= 1 && abs (head.y - tail.y) <= 1
  then tail
  else {x, y}

  where
  abs a | a > 0 = a
  abs a | otherwise = negate a

  x = case compare head.x tail.x of
    GT -> tail.x + 1
    EQ -> tail.x
    LT -> tail.x - 1
  y = case compare head.y tail.y of
    GT -> tail.y + 1
    EQ -> tail.y
    LT -> tail.y - 1

move :: Direction -> Position -> Position
move MoveUp {x, y} = {x, y: y+1}
move MoveDown {x, y} = {x, y: y-1}
move MoveLeft {x, y} = {x: x-1, y}
move MoveRight {x, y} = {x: x+1, y}

solution :: Array Movement -> Int
solution input = Set.size allTailPositions
  where 
  allTailPositions = Set.fromFoldable (_.tail <$> scanl fn initialState movements)
  initialState = {tail: {x: 0, y: 0}, head: {x: 0, y: 0}}
  movements = input # Array.concatMap \{count, direction} -> (1 .. count) <#> const direction
  fn {tail, head} direction = 
    let newHead = move direction head
    in {tail: chaseHead tail newHead, head: newHead}

ans :: forall m. MonadThrow String m => Array String -> m Int
ans input = parseInput input <#> solution 

run :: ExceptT String Aff Unit
run = getInput >>= ans >>= logShow
