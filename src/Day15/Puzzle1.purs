module Day15.Puzzle1 where

import AOC.Prelude

import Data.Array as Array
import Data.Set as Set
import Data.String as String
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Parsing.String as Parsing
import Parsing.String.Basic as Parsing

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day15.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") allText

testInput :: Array String
testInput = 
  [ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
  , "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
  , "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
  , "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
  , "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
  , "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
  , "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
  , "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
  , "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
  , "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
  , "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
  , "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
  , "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
  , "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
  ]

type Coord = { x :: Int, y :: Int }
type Sensor = { location :: Coord, closestBeacon :: Coord }
parseSensor :: ∀ m. MonadThrow String m => String -> m Sensor
parseSensor s = liftEither $ lmap parseErrorMessage $ runParser s $ do 
  { location: _, closestBeacon: _ } <$> parseLocation <*> parseBeacon
  
  where 
  parseLocation = Parsing.string "Sensor at " *> parseCoord <* Parsing.string ": "
  parseBeacon = Parsing.string "closest beacon is at " *> parseCoord

  parseCoord = do
    x <- Parsing.string "x=" *> Parsing.intDecimal 
    void $ Parsing.string ", "
    y <- Parsing.string "y=" *> Parsing.intDecimal
    pure {x, y}

data Interval a = EmptyInterval | PositiveInterval {minValue :: a, maxValue :: a}
derive instance Eq a => Eq (Interval a)
derive instance Ord a => Ord (Interval a)
  
data IntervalUnion a = Contiguous (Interval a) | Disjoint {lower :: Interval a, upper :: Interval a}
union :: ∀ a. Eq a => Ord a => Interval a -> Interval a -> IntervalUnion a
union a b = if disjoint a b then Disjoint {lower: min a b, upper: max a b} else Contiguous $ contiguousUnion a b
  where
  contiguousUnion EmptyInterval x = x
  contiguousUnion x EmptyInterval = x
  contiguousUnion (PositiveInterval a) (PositiveInterval b) = 
    PositiveInterval { minValue: min a.minValue b.minValue, maxValue: max a.maxValue b.maxValue }
  
  disjoint EmptyInterval _ = false
  disjoint _ EmptyInterval = false
  disjoint (PositiveInterval a) (PositiveInterval b) = a.minValue > b.maxValue || a.maxValue < b.minValue

excludedLocations :: Int -> Sensor -> Set Coord
excludedLocations row { location, closestBeacon } = allWithinDistance # Set.delete closestBeacon
  where
  maxDistance = manhattenDistance location closestBeacon - abs (location.y - row)
  manhattenDistance c1 c2 = abs (c1.x - c2.x) + abs (c1.y - c2.y)
  abs a 
    | a > 0 = a
    | otherwise = negate a

  allWithinDistance = Set.fromFoldable do
    x <- if maxDistance >= 0 then (location.x - maxDistance) .. (location.x + maxDistance) else []
    pure {x, y: row}

allExcludedLocations :: Int -> Array Sensor -> Set Coord
allExcludedLocations row = Array.foldMap $ excludedLocations row

solution :: Int -> Array Sensor -> Int
solution targetRow sensors = allExcludedLocations targetRow sensors # Set.size

ans :: ∀ m. MonadThrow String m => Int -> Array String -> m Int
ans targetRow input = traverse parseSensor input <#> solution targetRow

run :: ExceptT String Aff Unit
run = getInput >>= ans 2000000 >>= logShow
