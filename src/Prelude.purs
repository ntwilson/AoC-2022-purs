module AOC.Prelude (module Exports, (..), noteM) where

import Control.Alt ((<|>)) as Exports
import Control.Monad.Error.Class (class MonadThrow, liftEither, try, throwError) as Exports
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT) as Exports
import Data.Array.NonEmpty (NonEmptyArray) as Exports
import Data.Bifunctor (lmap) as Exports
import Data.Either (Either(..), note) as Exports
import Data.Filterable (filter, filterMap) as Exports
import Data.Foldable (fold, foldl, maximum, minimum, sum) as Exports
import Data.Function (on) as Exports
import Data.Interpolate (i) as Exports
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe) as Exports
import Data.Newtype (un, unwrap) as Exports
import Data.Set (Set) as Exports
import Data.String (Pattern(..)) as Exports
import Data.Traversable (for, for_, scanl, sequence, traverse, traverse_) as Exports
import Data.Tuple (Tuple(..)) as Exports
import Data.Tuple.Nested ((/\)) as Exports
import Data.Unfoldable (range) as Exports
import Debug (class DebugWarning, trace)
import Debug (trace) as Exports
import Effect (Effect) as Exports
import Effect.Aff (Aff, launchAff_) as Exports
import Effect.Class.Console (log, logShow) as Exports
import Effect.Exception (message) as Exports
import Parsing (parseErrorMessage, runParser) as Exports
import Prelude as Exports

infix 8 Exports.range as ..

noteM :: ∀ m a. Exports.MonadThrow String m => String -> Exports.Maybe a -> m a
noteM e x = Exports.liftEither (Exports.note e x)

iWantDebugInMyPackagesWithoutWarning :: DebugWarning => Int
iWantDebugInMyPackagesWithoutWarning = trace 0 \_ -> 0
