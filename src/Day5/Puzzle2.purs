module Day5.Puzzle2 where

import AOC.Prelude

import Data.Array as Array
import Data.CodePoint.Unicode (isAlphaNum)
import Data.String as String
import Data.String.CodeUnits as CUString
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Parsing (parseErrorMessage)
import Parsing as Parsing
import Parsing.String as Parsing
import Parsing.String.Basic as Parsing

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day5.txt"
  pure $ String.split (Pattern "\n") allText

testInput :: Array String
testInput = 
  [ "    [D]    "
  , "[N] [C]    "
  , "[Z] [M] [P]"
  , " 1   2   3 "
  , ""
  , "move 1 from 2 to 1"
  , "move 3 from 1 to 3"
  , "move 2 from 2 to 1"
  , "move 1 from 1 to 2"
  ]

type Instruction = { nCrates :: Int, sourceStack :: Int, destStack :: Int }
type Stack = { id :: Int, crates :: Array Char }
type Input = { instructions :: Array Instruction, stacks :: Array Stack }

parseInput :: ∀ m. MonadThrow String m => Array String -> m Input
parseInput input = do
  splitIndex <- 
    Array.findIndex (String.null <<< String.trim) input 
    # note "No blank line found in input to separate stacks and instructions"
    # liftEither

  let {before, after} = Array.splitAt splitIndex input
  stacks <- parseStackInput before
  instructions <- parseInstructions $ Array.filter (not String.null) after
  pure { stacks, instructions } 

  where 
  parseInstructions = liftEither <<< lmap parseErrorMessage <<< traverse (\s -> runParser s parseInstruction)

  parseInstruction = do
    nCrates <- Parsing.string "move " *> Parsing.intDecimal <|> Parsing.fail "Expected Int when parsing nCrates"
    sourceStack <- Parsing.string " from " *> Parsing.intDecimal <|> Parsing.fail "Expected Int when parsing sourceStack"
    destStack <- Parsing.string " to " *> Parsing.intDecimal <|> Parsing.fail "Expected Int when parsing destStack"
    pure { nCrates, sourceStack, destStack }

  parseStackInput stackInput = 
    let transposed = stackInput <#> CUString.toCharArray # Array.transpose <#> CUString.fromCharArray
    in transposed # Array.filter anyAlphaNum # traverse parseStack

  anyAlphaNum = String.dropWhile (not isAlphaNum) >>> not String.null

  parseStack s = liftEither $ lmap parseErrorMessage $ runParser s do
    Parsing.skipSpaces
    crates <- Array.many Parsing.letter
    id <- Parsing.intDecimal <|> Parsing.fail "Expected Int when parsing stack id"
    pure { crates, id }

evaluateInstruction :: ∀ m. MonadThrow String m => Array Stack -> Instruction -> m (Array Stack)
evaluateInstruction stacks { nCrates, destStack, sourceStack } = do
  source <- Array.find (_.id >>> (_ == sourceStack)) stacks # noteM (i"No stack found with id "sourceStack)
  dest <- Array.find (_.id >>> (_ == destStack)) stacks # noteM (i"No stack found with id "destStack)
  {newDest, newSource} <- moveCrates nCrates source dest

  pure (stacks <#> \stack -> 
    if stack.id == newSource.id then newSource
    else if stack.id == newDest.id then newDest
    else stack
  )

  where 
  moveCrates nCrates source dest = do
    let 
      {before: cratesToMove, after: sourceCrates} = Array.splitAt nCrates source.crates
      newDest = dest { crates = cratesToMove <> dest.crates }
      newSource = source { crates = sourceCrates }

    pure {newDest, newSource}

ans :: ∀ m. Bind m => Monad m => MonadThrow String m => Input -> m String
ans { instructions, stacks } = do
  stacks <- Array.foldM evaluateInstruction stacks instructions
  pure (stacks # Array.mapMaybe (_.crates >>> Array.head) # CUString.fromCharArray)

run :: ExceptT String Aff Unit
run = getInput >>= parseInput >>= ans >>= logShow
