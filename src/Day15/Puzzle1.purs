module Day15.Puzzle1 where

import AOC.Prelude

import Data.Array as Array
import Data.Filterable (partition)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Semigroup.Foldable as F1
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

type PositiveInterval a = {minValue :: a, maxValue :: a}
type Interval a = Maybe (PositiveInterval a)
type IntervalUnion a = List (PositiveInterval a)
appendInterval :: ∀ a. Eq a => Ord a => Interval a -> IntervalUnion a -> IntervalUnion a
appendInterval x xs = case x, NonEmptyList.fromList intersections.yes of
  Nothing, _ -> xs
  Just x, Nothing -> x:xs
  Just x, Just overlapping -> contiguousUnion (NonEmptyList.cons x overlapping) : intersections.no 
  
  where
  intersects Nothing _ = false
  intersects (Just a) b = 
    between b.minValue b.maxValue a.minValue || between b.minValue b.maxValue a.maxValue 
      || between a.minValue a.maxValue b.minValue || between a.minValue a.maxValue b.maxValue 

  intersections = partition (intersects x) xs

  contiguousUnion :: NonEmptyList (PositiveInterval a) -> PositiveInterval a
  contiguousUnion xs = {minValue: F1.minimum (_.minValue <$> xs), maxValue: F1.maximum (_.maxValue <$> xs)}

union :: ∀ a. Eq a => Ord a => IntervalUnion a -> IntervalUnion a -> IntervalUnion a
union xs ys = foldl (\zs z -> appendInterval (Just z) zs) xs ys

intervalSize :: Interval Int -> Int
intervalSize Nothing = 0
intervalSize (Just {minValue, maxValue}) = maxValue - minValue + 1

unionSize :: IntervalUnion Int -> Int
unionSize xs = sum $ (intervalSize <<< Just) <$> xs

removeIntervalValue :: ∀ a. Ord a => Ring a => a -> Interval a -> IntervalUnion a
removeIntervalValue _ Nothing = Nil
removeIntervalValue x (Just xs@{minValue, maxValue}) 
  | x < minValue || x > maxValue = xs:Nil
  | otherwise = 
    {minValue, maxValue: x-one} : {minValue: x+one, maxValue} : Nil 
      # filter (\{minValue, maxValue} -> minValue <= maxValue)

excludedLocations :: Int -> Sensor -> IntervalUnion Int
excludedLocations row { location, closestBeacon } = 
  if row == closestBeacon.y 
  then removeIntervalValue closestBeacon.x allWithinDistance 
  else appendInterval allWithinDistance Nil

  where
  -- use this for tracing
  -- msg x = i"Sensor: "(show location)", beacon: "(show closestBeacon)", excluded locations: "(show x) :: String
  maxDistance = manhattenDistance location closestBeacon - abs (location.y - row)
  manhattenDistance c1 c2 = abs (c1.x - c2.x) + abs (c1.y - c2.y)
  abs a 
    | a > 0 = a
    | otherwise = negate a

  allWithinDistance = 
    if maxDistance >= 0 
    then Just {minValue: (location.x - maxDistance), maxValue: (location.x + maxDistance)} 
    else Nothing

allExcludedLocations :: Int -> Array Sensor -> IntervalUnion Int
allExcludedLocations row sensors = foldl union Nil (excludedLocations row <$> sensors)
  -- where
  -- use this folding function for tracing
  -- traceUnion l r = 
  --   let x = union l r
  --   in trace (i"union of "(show l)" and "(show r)" is "(show x) :: String) \_ -> x

solution :: Int -> Array Sensor -> Int
solution targetRow sensors = allExcludedLocations targetRow sensors # unionSize

ans :: ∀ m. MonadThrow String m => Int -> Array String -> m Int
ans targetRow input = traverse parseSensor input <#> solution targetRow

run :: ExceptT String Aff Unit
run = getInput >>= ans 2000000 >>= logShow

