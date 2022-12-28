module Day13.Puzzle2 where

import AOC.Prelude

import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as String
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
  decodeJson json 
    | Right i <- decodeJson json = pure $ Single i
    | Right xs <- decodeJson json = pure $ Compound xs
    | otherwise = throwError $ Named (stringify json) $ TypeMismatch "Int or Array"

split :: ∀ a. Eq a => a -> Array a -> Array (NonEmptyArray a)
split x xs = xs # Array.groupBy (\a b -> a /= x && b /= x) # Array.filter (_ /= (NonEmptyArray.singleton x))

parseInput :: ∀ m. MonadThrow String m => Array String -> m (Array Value)
parseInput lines = 
  lines 
    # Array.filter (not String.null)
    # traverse (parseJson >=> decodeJson) 
    # lmap printJsonDecodeError 
    # liftEither

solution :: ∀ m. MonadThrow String m => Array Value -> m Int
solution values = do
  i <- indexOfPacket2 
  j <- indexOfPacket6 
  pure $ i * j

  where
  completeList = Array.sort $ plusDividerPackets values
  packet2 = Compound $ [Single 2]
  packet6 = Compound $ [Single 6]
  dividerPackets = [packet2, packet6]
  plusDividerPackets = (dividerPackets <> _)

  indexOfPacket2 = findIndex packet2
  indexOfPacket6 = findIndex packet6
  findIndex item = do
    i <- completeList # Array.findIndex (_ == item) # noteM (i"Unable to locate item "(show item)". Perhaps there's a code bug?")
    pure $ i + 1


ans :: ∀ m. MonadThrow String m => Array String -> m Int
ans = parseInput >=> solution

run :: ExceptT String Aff Unit
run = getInput >>= ans >>= logShow
