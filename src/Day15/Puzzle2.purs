module Day15.Puzzle2 where

import AOC.Prelude

import Data.Array as Array
import Data.Filterable (partition)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Semigroup.Foldable as F1
import Data.String as String
import Data.String.CodeUnits as CU.String
import Effect.Class (class MonadEffect)
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
appendInterval :: ∀ a. Ord a => Ring a => Interval a -> IntervalUnion a -> IntervalUnion a
appendInterval x xs = case x, NonEmptyList.fromList intersections.yes of
  Nothing, _ -> xs
  Just x, Nothing -> x:xs
  Just x, Just overlapping -> contiguousUnion (NonEmptyList.cons x overlapping) : intersections.no 
  
  where
  intersectsOrAdjacent Nothing _ = false
  intersectsOrAdjacent (Just a) b = intersectsWith a b || intersectsWith b a

  intersectsWith a b = 
    between (b.minValue - one) (b.maxValue + one) a.minValue || between (b.minValue - one) (b.maxValue + one) a.maxValue 

  intersections = partition (intersectsOrAdjacent x) xs

  contiguousUnion :: NonEmptyList (PositiveInterval a) -> PositiveInterval a
  contiguousUnion xs = {minValue: F1.minimum (_.minValue <$> xs), maxValue: F1.maximum (_.maxValue <$> xs)}

union :: ∀ a. Ord a => Ring a => IntervalUnion a -> IntervalUnion a -> IntervalUnion a
union xs ys = foldl (\zs z -> appendInterval (Just z) zs) xs ys

intervalSize :: Interval Int -> Int
intervalSize Nothing = 0
intervalSize (Just {minValue, maxValue}) = maxValue - minValue + 1

unionSize :: IntervalUnion Int -> Int
unionSize xs = sum $ (intervalSize <<< Just) <$> xs

intervalContains :: ∀ a. Ord a => a -> Interval a -> Boolean
intervalContains _ Nothing = false
intervalContains x (Just {minValue, maxValue}) = between minValue maxValue x

unionContains :: ∀ a. Ord a => a -> IntervalUnion a -> Boolean
unionContains x = any (Just >>> intervalContains x)

removeIntervalValue :: ∀ a. Ord a => Ring a => a -> Interval a -> IntervalUnion a
removeIntervalValue _ Nothing = Nil
removeIntervalValue x (Just xs@{minValue, maxValue}) 
  | x < minValue || x > maxValue = xs:Nil
  | otherwise = 
    {minValue, maxValue: x-one} : {minValue: x+one, maxValue} : Nil 
      # filter (\{minValue, maxValue} -> minValue <= maxValue)

excludedLocations :: Int -> Sensor -> IntervalUnion Int
excludedLocations row { location, closestBeacon } = appendInterval allWithinDistance Nil
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

allExcludedLocations :: Array Sensor -> Int -> IntervalUnion Int
allExcludedLocations sensors row = foldl union Nil (excludedLocations row <$> sensors)
  -- where
  -- use this folding function for tracing
  -- traceUnion l r = 
  --   let x = union l r
  --   in trace (i"union of "(show l)" and "(show r)" is "(show x) :: String) \_ -> x

solution :: Int -> Array Sensor -> Array { x :: Int, y :: Int }
solution maxBound sensors = 
  filterMap possibleBeacon $ Array.mapWithIndex {y:_, vals:_} $ allExcludedLocations sensors <$> (minBound .. maxBound)
  where 
  possibleBeacon {y, vals} = case List.sort vals of 
    a:b:_ | b.minValue - a.maxValue > 1 -> Just {y, x: a.maxValue + 1}
    a:Nil | a.minValue > 0 -> Just {y, x: 0}
    a:Nil | a.maxValue < maxBound -> Just {y, x: maxBound}
    _ -> Nothing
  minBound = 0

ans :: ∀ m. MonadThrow String m => Int -> Array String -> m (Array { x :: Int, y :: Int })
ans maxRow input = traverse parseSensor input <#> solution maxRow

run :: ExceptT String Aff Unit
run = getInput >>= ans 4000000 >>= logShow

renderRow :: Int -> Int -> Array Sensor -> IntervalUnion Int -> String
renderRow maxBound y sensors excluded = CU.String.fromCharArray do
  x <- 0 .. maxBound
  pure $ 
    if sensors # any (\{location} -> location == {x, y}) then 'S'
    else if sensors # any (\{closestBeacon} -> closestBeacon == {x, y}) then 'B'
    else if unionContains x excluded then '#'
    else ' '

createGrid :: Int -> Array Sensor -> Array String
createGrid maxBound sensors = do
  row <- 0 .. maxBound
  let excluded = allExcludedLocations sensors row
  pure $ renderRow maxBound row sensors excluded

printGrid :: ∀ m. MonadEffect m => Int -> Array Sensor -> m Unit
printGrid maxBound sensors = do
  for_ (0 .. maxBound :: Array _) \row -> do
    let excluded = allExcludedLocations sensors row
    log $ renderRow maxBound row sensors excluded
