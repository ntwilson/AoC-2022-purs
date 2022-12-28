module Day13.Puzzle1 where

import AOC.Prelude

import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (fst, snd)
import Node.Encoding as Encoding
import Node.FS.Aff as FS

getInput :: ExceptT String Aff (Array String)
getInput = 
  String.split (Pattern "\n") <$> (withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day13.txt")

testInput :: Array String
testInput = 
  [ "[1,1,3,1,1]"
  , "[1,1,5,1,1]"
  , ""
  , "[[1],[2,3,4]]"
  , "[[1],4]"
  , ""
  , "[9]"
  , "[[8,7,6]]"
  , ""
  , "[[4,4],4,4]"
  , "[[4,4],4,4,4]"
  , ""
  , "[7,7,7,7]"
  , "[7,7,7]"
  , ""
  , "[]"
  , "[3]"
  , ""
  , "[[[]]]"
  , "[[]]"
  , ""
  , "[1,[2,[3,[4,[5,6,7]]]],8,9]"
  , "[1,[2,[3,[4,[5,6,0]]]],8,9]"
  ]

data Value = Single Int | Compound (Array Value)
derive instance Eq Value
derive instance Generic Value _
instance Show Value where show v = genericShow v
instance Ord Value where
  compare (Single l) (Single r) = compare l r
  compare (Single l) (Compound r) = compare (Compound [Single l]) (Compound r)
  compare (Compound l) (Single r) = compare (Compound l) (Compound [Single r])
  compare (Compound l) (Compound r) = compare l r

instance DecodeJson Value where
  decodeJson json = 
    (Single <$> decodeJson json) <|> (Compound <$> decodeJson json) 
      <|> (throwError $ Named (stringify json) $ TypeMismatch "Int or Array")

split :: ∀ a. Eq a => a -> Array a -> Array (NonEmptyArray a)
split x xs = xs # Array.groupBy (\a b -> a /= x && b /= x) # Array.filter (_ /= (NonEmptyArray.singleton x))

parseInput :: ∀ m. MonadThrow String m => Array String -> m (Array { left :: Value, right :: Value })
parseInput lines = lines # split "" # traverse parsePair >>= traverse decodePair
  where 
  parsePair = parseArrayPair <<< NonEmptyArray.toArray
  parseArrayPair [left,right] = pure {left, right}
  parseArrayPair xs = throwError (i"expected "(show xs)" to only contain two elements")

  decodePair {left, right} = liftEither $ lmap printJsonDecodeError do
    {left:_, right:_} <$> decodeValue left <*> decodeValue right

  decodeValue = parseJson >=> decodeJson

solution :: Array { left :: Value , right :: Value } -> Int
solution = Array.mapWithIndex (\i x -> (i+1) /\ x) >>> Array.filter (snd >>> isLT) >>> map fst >>> sum
  where
  isLT {left, right} = left <= right

ans :: ∀ m. MonadThrow String m => Array String -> m Int
ans = parseInput >>> map solution

run :: ExceptT String Aff Unit
run = getInput >>= ans >>= logShow