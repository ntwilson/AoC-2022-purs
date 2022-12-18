module Day4.Puzzle2 where

import AOC.Prelude

import Control.Monad.Error.Class (liftEither)
import Data.Array as Array
import Data.String as String
import Data.Tuple (uncurry)
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Parsing (parseErrorMessage)
import Parsing.String as Parser
import Parsing.String.Basic as Parser

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day4.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") allText

testInput :: Array String
testInput = 
  [ "2-4,6-8"
  , "2-3,4-5"
  , "5-7,7-9"
  , "2-8,3-7"
  , "6-6,4-6"
  , "2-6,4-8"
  ]

parsePair :: forall m. MonadThrow String m => String -> m (Tuple (Array Int) (Array Int))
parsePair s = liftEither $ lmap parseErrorMessage $ runParser s $ do
  elf1 <- parseRange
  _ <- Parser.string ","
  elf2 <- parseRange
  pure (elf1 /\ elf2)

  where
  parseRange = do
    l <- Parser.intDecimal
    _ <- Parser.string "-"
    r <- Parser.intDecimal
    pure (l .. r)

parseInput :: forall m. Applicative m => MonadThrow String m => 
  Array String -> m (Array (Tuple (Array Int) (Array Int)))
parseInput = traverse parsePair

pairIsTarget :: Tuple (Array Int) (Array Int) -> Boolean
pairIsTarget = uncurry fn
  where
  fn range1 range2 = Array.length range1 + Array.length range2 /= n
    where
    n = Array.length $ Array.union range1 range2

ans :: forall m. Functor m => Applicative m => MonadThrow String m => Array String -> m Int
ans input = parseInput input <#> Array.filter pairIsTarget <#> Array.length

run :: ExceptT String Aff Unit
run = getInput >>= ans >>= logShow
