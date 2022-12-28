module Day12.Puzzle1 where

import AOC.Prelude

import Control.Monad.State (State, evalState, state)
import Control.Monad.State as State
import Data.Array as Array
import Data.Enum (succ)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as CU.String
import Node.Encoding as Encoding
import Node.FS.Aff as FS

testInput :: Array String
testInput = 
  [ "Sabqponm"
  , "abcryxxl"
  , "accszExk"
  , "acctuvwj"
  , "abdefghi"
  ]

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day12.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") allText

type Height = Char
type Position = {x::Int, y::Int, z::Height}
type Map = Array (Array Position)
parseInput :: ∀ m. MonadThrow String m => Array String -> m {map :: Map, startingLocation :: Position}
parseInput input = {map, startingLocation:_} <$> startingLocation
  where
  linesToHeights = CU.String.toCharArray <$> input
  map = Array.mapWithIndex (\y -> Array.mapWithIndex {y,x:_, z:_}) linesToHeights

  startingLocation = 
    Array.findMap (Array.findMap (\pos@{z} -> if z == 'S' then Just (pos {z = 'a'}) else Nothing)) map
      # noteM "Unable to find starting position 'S'"
  
isMoveValid :: Position -> Position -> Boolean
isMoveValid {z:location} {z:target} 
  | target == 'E' = (location == 'z' || location == 'y')
  | otherwise = location >= target || succ location == Just target

type Path = NonEmptyList Position

initialVisited :: Position -> Set Position
initialVisited startingLocation = Set.singleton startingLocation
initialPaths :: Position -> List Path
initialPaths startingLocation = List.singleton (NonEmptyList.singleton startingLocation)

isMoveWise :: Set Position -> Position -> Position -> Boolean 
isMoveWise visitedLocations location target = isMoveValid location target && not Set.member target visitedLocations

visitLocation :: Position -> State (Set Position) Unit
visitLocation pos = state \s -> unit /\ Set.insert pos s

evaluateNextMoveForPath :: Map -> Path -> State (Set Position) (Maybe (List Path))
evaluateNextMoveForPath map path = do
  s <- State.get
  let 
    location = NonEmptyList.head path
    up = map # index (location.y + 1) >>= index location.x
    down = map # index (location.y - 1) >>= index location.x
    left = map # index location.y >>= index (location.x - 1)
    right = map # index location.y >>= index (location.x + 1)
    extensions = List.catMaybes (up:down:left:right:Nil) # List.filter (isMoveWise s location)

  if List.null extensions 
  then pure Nothing 
  else Just <$> do
    traverse_ visitLocation extensions
    pure (extensions <#> \loc -> NonEmptyList.cons loc path)

  where 
  index :: ∀ a. Int -> Array a -> Maybe a
  index = flip Array.index

evaluateNextMove :: Map -> List Path -> State (Set Position) (List Path)
evaluateNextMove map paths = do
  newPaths <- traverse (evaluateNextMoveForPath map) paths
  pure $ List.concat $ List.catMaybes newPaths

completedPath :: List Path -> Maybe Path
completedPath = List.find (NonEmptyList.head >>> _.z >>> (_ == 'E'))

solution :: {startingLocation :: Position, map :: Map} -> Int
solution {startingLocation, map} = NonEmptyList.length finalPath - 1
  where
  finalPath = evalState (go startingPaths) (initialVisited startingLocation)
  go move = do
    nextMove <- evaluateNextMove map move 
    case completedPath nextMove of
      Just x -> pure x
      Nothing -> go nextMove
    
  startingPaths = initialPaths startingLocation 

ans :: ∀ m. MonadThrow String m => Array String -> m Int
ans input = parseInput input <#> solution

run :: ExceptT String Aff Unit
run = getInput >>= ans >>= logShow


