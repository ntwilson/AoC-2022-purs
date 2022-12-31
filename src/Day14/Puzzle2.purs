module Day14.Puzzle2 where

import AOC.Prelude

import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as CU.String
import Effect.Class (class MonadEffect)
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Parsing.Combinators as Parsing
import Parsing.String as Parsing
import Parsing.String.Basic as Parsing
import Partial.Unsafe (unsafePartial)

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day14.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") allText

testInput :: Array String
testInput =
  [ "498,4 -> 498,6 -> 496,6"
  , "503,4 -> 502,4 -> 502,9 -> 494,9"
  ]

data MapTile = Rock | Sand
derive instance Eq MapTile
renderMapTile :: MapTile -> Char
renderMapTile Rock = '#'
renderMapTile Sand = 'o'

type Coord = {x :: Int, y :: Int}

type MapGrid = Map Coord MapTile

renderMapGrid :: MapGrid -> Array String
renderMapGrid grid = CU.String.fromCharArray <$> do
  y <- yBounds
  pure do
    x <- xBounds
    pure $ case Map.lookup {x, y} grid of 
      Just tile -> renderMapTile tile
      Nothing -> ' '

  where 
  yBounds = boundsFor yVals
  xBounds = boundsFor xVals
  boundsFor vals = case minimum vals, maximum vals of
    Just min, Just max -> (min .. max)
    _, _ -> []
  
  yVals = axisVals (_.y)
  xVals = axisVals (_.x)
  axisVals selector = Map.keys grid # Set.map selector

parseLine :: ∀ m. MonadThrow String m => String -> m (List Coord)
parseLine line = liftEither $ lmap parseErrorMessage $ runParser line $
  parsePair `Parsing.sepBy` Parsing.string " -> "
  where 
  parsePair = do
    x <- Parsing.intDecimal <* Parsing.string ","
    y <- Parsing.intDecimal
    pure {x, y}

parseGrid :: ∀ m. MonadThrow String m => Array String -> m MapGrid
parseGrid input = do
  lines <- traverse parseLine input
  pure $ Map.fromFoldable do
    line <- lines
    {left, right} <- pairwise $ Array.fromFoldable line
    x <- left.x .. right.x
    y <- left.y .. right.y
    pure ({x,y} /\ Rock)

pairwise :: ∀ a. Array a -> Array {left :: a, right :: a}
pairwise xs 
  | Array.length xs < 2 = []
  | otherwise = do
    i <- (0 .. (Array.length xs - 2)) 
    let maybeAns = {left: _, right: _} <$> Array.index xs i <*> Array.index xs (i+1)
    pure $ unsafePartial $ fromJust maybeAns

printGrid :: ∀ m. MonadEffect m => MapGrid -> m Unit
printGrid = renderMapGrid >>> traverse_ log

dropSand :: MapGrid -> Maybe MapGrid
dropSand grid = do
  rest <- findSandRestingLocation entryPoint
  pure $ Map.insert rest Sand grid

  where
  findSandRestingLocation {x, y} = 
    if spaceTaken {x, y} then Nothing
    else if not spaceTaken {x, y:y+1} then findSandRestingLocation {x, y:y+1}
    else if not spaceTaken {x: x-1, y: y+1} then findSandRestingLocation {x: x-1, y: y+1}
    else if not spaceTaken {x: x+1, y: y+1} then findSandRestingLocation {x: x+1, y: y+1}
    else Just {x, y}

  spaceTaken {x, y} = Map.member {x, y} grid || y >= floor

  floor = case maximum (Map.keys justRocks # Set.map _.y) of 
    Just maxY -> maxY + 2
    Nothing -> 2
  justRocks = Map.filter (_ == Rock) grid
  entryPoint = {x: 500, y: 0}
  

dropAsMuchSandAsPossible :: MapGrid -> MapGrid
dropAsMuchSandAsPossible grid = case dropSand grid of 
  Nothing -> grid
  Just newGrid -> dropAsMuchSandAsPossible newGrid

solution :: MapGrid -> Int
solution grid = dropAsMuchSandAsPossible grid # Map.values # List.filter (_ == Sand) # List.length

ans :: ∀ m. MonadThrow String m => Array String -> m Int
ans = parseGrid >>> map solution

run :: ExceptT String Aff Unit
run = getInput >>= ans >>= logShow
