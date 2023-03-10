module Day7.Puzzle1 where

import AOC.Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple as Tuple
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Parsing (ParserT, parseErrorMessage)
import Parsing.Combinators as Parsing
import Parsing.String as Parsing
import Parsing.String.Basic as Parsing

getInput :: ExceptT String Aff (Array String)
getInput = do
  allText <- withExceptT message $ ExceptT $ try $ FS.readTextFile Encoding.UTF8 "inputs/Day7.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") allText

-- - / (dir)
--   - a (dir)
--     - e (dir)
--       - i (file, size=584)
--     - f (file, size=29116)
--     - g (file, size=2557)
--     - h.lst (file, size=62596)
--   - b.txt (file, size=14848514)
--   - c.dat (file, size=8504156)
--   - d (dir)
--     - j (file, size=4060174)
--     - d.log (file, size=8033020)
--     - d.ext (file, size=5626152)
--     - k (file, size=7214296)
testInput :: Array String 
testInput =
  [ "$ cd /"
  , "$ ls"
  , "dir a"
  , "14848514 b.txt"
  , "8504156 c.dat"
  , "dir d"
  , "$ cd a"
  , "$ ls"
  , "dir e"
  , "29116 f"
  , "2557 g"
  , "62596 h.lst"
  , "$ cd e"
  , "$ ls"
  , "584 i"
  , "$ cd .."
  , "$ cd .."
  , "$ cd d"
  , "$ ls"
  , "4060174 j"
  , "8033020 d.log"
  , "5626152 d.ext"
  , "7214296 k"
  ]

-- we only need to capture changes to the current directory, plus any files listed in the current directory.
--`ls` commands are irrelevant, as are directories listed when doing `ls`.
data Input = NavigateIn String | NavigateOut | FileSize Int
derive instance Eq Input
derive instance Generic Input _
instance Show Input where show = genericShow

parseInput :: ∀ m. MonadThrow String m => String -> m (Maybe Input)
parseInput s = liftEither $ lmap parseErrorMessage $ runParser s $ do
  parseNavigateOut <|> parseNavigateIn <|> parseFileSize <|> parseLSCommand <|> parseDir
  where
  parseNavigateOut = Parsing.try (Parsing.string "$ cd .." $> Just NavigateOut)
  parseNavigateIn = Parsing.try (Parsing.string "$ cd " *> restOfLine <#> NavigateIn <#> Just) 
  parseFileSize = Parsing.try (Parsing.intDecimal <* Parsing.space <* restOfLine) <#> FileSize <#> Just

  parseLSCommand = Parsing.try $ Parsing.string "$ ls" $> Nothing
  parseDir = Parsing.try $ (Parsing.string "dir " *> restOfLine) $> Nothing

  restOfLine :: ParserT String Identity String
  restOfLine = Parsing.anyTill Parsing.eof <#> Tuple.fst

parseAllInput :: ∀ m. MonadThrow String m => Array String -> m (Array Input)
parseAllInput lines = case Array.uncons lines of 
  Just {head: "$ cd /", tail} -> Array.catMaybes <$> (traverse parseInput tail)
  Just {head} -> throwError (i"Expecting input to start with 'cd /', but starts with'"head"'. Unable to parse input without starting in the home directory")
  Nothing -> throwError "Unable to parse empty input"

data FSItem = Dir (List FSItem) | File Int
derive instance Generic FSItem _
instance Show FSItem where show x = genericShow x

sizeOf :: FSItem -> Int
sizeOf (Dir xs) = sum $ (sizeOf <$> xs)
sizeOf (File size) = size

buildFileSystem :: List Input -> FSItem
buildFileSystem input = Dir $ consumeDir input 
  where
  -- consumes until you hit NavigateOut and then returns the consumed stuff plus the remainder
  consumeDir :: List Input -> List FSItem
  consumeDir i = let {this} = go Nil i in this
    where
    go :: (List FSItem) -> List Input -> { this :: List FSItem, next :: List Input }
    go this input = case input of
      FileSize i : rest -> go (File i : this) rest
      NavigateIn _ : rest -> 
        let {this: dir, next} = go Nil rest
        in go (Dir dir : this) next
      NavigateOut : next -> { this, next }
      Nil -> { this, next: Nil }

solution :: FSItem -> Int
solution (File _) = 0
solution (Dir xs) = sum $ filter (_ <= 100000) $ allDirectories xs
  where
  allDirectories ((Dir xs) : rest) = sizeOf (Dir xs) : allDirectories xs <> allDirectories rest
  allDirectories (File _ : rest) = allDirectories rest
  allDirectories Nil = Nil

ans :: ∀ m. MonadThrow String m => Array String -> m Int
ans xs = solution <<< buildFileSystem <<< List.fromFoldable <$> parseAllInput xs

run :: ExceptT String Aff Unit
run = getInput >>= ans >>= logShow