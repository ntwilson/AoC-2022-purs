module Day11.Puzzle2 where

import AOC.Prelude

import Control.Monad.State (StateT, state)
import Control.Monad.State as State
import Control.Monad.State as StateT
import Control.Monad.Trampoline (Trampoline, runTrampoline)
import Control.Plus (empty)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.CatQueue (CatQueue)
import Data.CatQueue as CatQueue
import Data.Foldable (product)
import Effect.Class (class MonadEffect)

infixr 6 CatQueue.cons as :

type MonkeyID = Int
type WorryLevel = BigInt

type Monkey = 
  { items :: CatQueue WorryLevel 
  , operation :: WorryLevel -> WorryLevel
  , behavior :: { denom :: WorryLevel, tt :: MonkeyID, ff :: MonkeyID }
  , itemsHandled :: BigInt
  }

type MonkeyState = { divisor :: WorryLevel, monkeys :: Array Monkey }

testInput :: MonkeyState
testInput = 
  { divisor: BigInt.fromInt $ 13 * 17 * 19 * 23
  , monkeys:
    [ { items: BigInt.fromInt <$> 79:98:empty
      , operation: (_ * BigInt.fromInt 19)
      , behavior: {denom: BigInt.fromInt 23, tt: 2, ff: 3}
      , itemsHandled: BigInt.fromInt 0
      }
    , { items: BigInt.fromInt <$> 54:65:75:74:empty
      , operation: (_ + BigInt.fromInt 6)
      , behavior: {denom: BigInt.fromInt 19, tt: 2, ff: 0}
      , itemsHandled: BigInt.fromInt 0
      }
    , { items: BigInt.fromInt <$> 79:60:97:empty
      , operation: \i -> i * i
      , behavior: {denom: BigInt.fromInt 13, tt: 1, ff: 3}
      , itemsHandled: BigInt.fromInt 0
      }
    , { items: BigInt.fromInt <$> 74:empty
      , operation: (_ + BigInt.fromInt 3)
      , behavior: {denom: BigInt.fromInt 17, tt: 0, ff: 1}
      , itemsHandled: BigInt.fromInt 0
      }
    ]
  }

realInput :: MonkeyState 
realInput = 
  { divisor: BigInt.fromInt $ 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19
  , monkeys:
    [ { items: BigInt.fromInt <$> 64:empty
      , operation: (_ * BigInt.fromInt 7)
      , behavior: {denom: BigInt.fromInt 13, tt: 1, ff: 3}
      , itemsHandled: BigInt.fromInt 0
      }
    , { items: BigInt.fromInt <$> 60:84:84:65:empty
      , operation: (_ + BigInt.fromInt 7)
      , behavior: {denom: BigInt.fromInt 19, tt: 2, ff: 7}
      , itemsHandled: BigInt.fromInt 0
      } 
    , { items: BigInt.fromInt <$> 52:67:74:88:51:61:empty
      , operation: (_ * BigInt.fromInt 3)
      , behavior: {denom: BigInt.fromInt 5, tt: 5, ff: 7}
      , itemsHandled: BigInt.fromInt 0
      }
    , { items: BigInt.fromInt <$> 67:72:empty
      , operation: (_ + BigInt.fromInt 3)
      , behavior: {denom: BigInt.fromInt 2, tt: 1, ff: 2}
      , itemsHandled: BigInt.fromInt 0
      }
    , { items: BigInt.fromInt <$> 80:79:58:77:68:74:98:64:empty
      , operation: \i -> i * i
      , behavior: {denom: BigInt.fromInt 17, tt: 6, ff: 0}
      , itemsHandled: BigInt.fromInt 0
      }
    , { items: BigInt.fromInt <$> 62:53:61:89:86:empty
      , operation: (_ + BigInt.fromInt 8)
      , behavior: {denom: BigInt.fromInt 11, tt: 4, ff: 6}
      , itemsHandled: BigInt.fromInt 0
      }
    , { items: BigInt.fromInt <$> 86:89:82:empty
      , operation: (_ + BigInt.fromInt 2)
      , behavior: {denom: BigInt.fromInt 7, tt: 3, ff: 0}
      , itemsHandled: BigInt.fromInt 0
      }
    , { items: BigInt.fromInt <$> 92:81:70:96:69:84:83:empty
      , operation: (_ + BigInt.fromInt 4)
      , behavior: {denom: BigInt.fromInt 3, tt: 4, ff: 5}
      , itemsHandled: BigInt.fromInt 0
      }
    ]
  }

monkeyToThrowTo :: Monkey -> WorryLevel -> MonkeyID 
monkeyToThrowTo {behavior: {denom, tt, ff}} item = if item `mod` denom == zero then tt else ff

type MonkeyBusiness = StateT MonkeyState Trampoline

throwItem :: WorryLevel -> MonkeyID -> MonkeyBusiness Unit
throwItem item id = state \{divisor, monkeys} -> unit /\ fromMaybe {divisor, monkeys} do
  newMonkeys <- monkeys # Array.alterAt id (\monkey -> Just $ monkey { items = CatQueue.snoc monkey.items item }) 
  pure {divisor, monkeys: newMonkeys}

passItem :: MonkeyID -> MonkeyBusiness (Maybe WorryLevel)
passItem id = do
  {divisor, monkeys} <- State.get
  let 
    result = do
      monkey <- Array.index monkeys id
      item /\ items <- CatQueue.uncons monkey.items
      let newMonkey = monkey { items = items, itemsHandled = monkey.itemsHandled + one }
      state <- Array.alterAt id (\_ -> Just newMonkey) monkeys
      pure { item, monkey: newMonkey, state }

  case result of 
    Just {item, monkey, state} -> do
      State.put {divisor, monkeys: state}
      let worry = monkey.operation item `mod` divisor
      throwItem worry (monkeyToThrowTo monkey worry)
      pure $ Just worry 
    Nothing -> pure Nothing 
      
evaluateWhile :: ∀ s m a. Monad m => (s -> a -> Boolean) -> StateT s m a -> StateT s m a
evaluateWhile predicate st = do
  x <- st
  s <- State.get
  if predicate s x then evaluateWhile predicate st else pure x

evaluateMonkey :: MonkeyID -> MonkeyBusiness Unit
evaluateMonkey id = void $ evaluateWhile (const isJust) (passItem id)

evaluateRound :: MonkeyBusiness Unit
evaluateRound = do
  {monkeys} <- State.get
  (Array.range 0 $ Array.length monkeys - 1) # traverse_ evaluateMonkey

ans :: MonkeyState -> BigInt
ans inputs = finalState.monkeys <#> _.itemsHandled # Array.sortBy (comparing negate) # Array.take 2 # product
-- ans inputs = finalState.monkeys # Array.concatMap (_.items >>> Array.fromFoldable) -- # maximum
  where
  finalState = StateT.execStateT (runNRounds 10_000) inputs # runTrampoline
  runNRounds n = Array.range 1 n # traverse_ (const evaluateRound)

run :: ∀ m. MonadEffect m => m Unit
run = logShow $ ans realInput 
