module Day11.Puzzle1 where

import AOC.Prelude

import Control.Monad.State (State, state)
import Control.Monad.State as State
import Control.Plus (empty)
import Data.Array as Array
import Data.CatQueue (CatQueue)
import Data.CatQueue as CatQueue
import Data.Foldable (product)
import Effect.Class (class MonadEffect)

infixr 6 CatQueue.cons as :

type MonkeyID = Int
type WorryLevel = Int

type Monkey = 
  { items :: CatQueue WorryLevel 
  , operation :: WorryLevel -> WorryLevel
  , monkeyToThrowTo :: WorryLevel -> MonkeyID
  , itemsHandled :: Int
  }

testInput :: Array Monkey
testInput = 
  [ { items: 79:98:empty
    , operation: (_ * 19)
    , monkeyToThrowTo: \i -> if i `mod` 23 == 0 then 2 else 3
    , itemsHandled: 0
    }
  , { items: 54:65:75:74:empty
    , operation: (_ + 6)
    , monkeyToThrowTo: \i -> if i `mod` 19 == 0 then 2 else 0
    , itemsHandled: 0
    }
  , { items: 79:60:97:empty
    , operation: \i -> i * i
    , monkeyToThrowTo: \i -> if i `mod` 13 == 0 then 1 else 3
    , itemsHandled: 0
    }
  , { items: 74:empty
    , operation: (_ + 3)
    , monkeyToThrowTo: \i -> if i `mod` 17 == 0 then 0 else 1
    , itemsHandled: 0
    }
  ]

realInput :: Array Monkey 
realInput = 
  [ { items: 64:empty
    , operation: (_ * 7)
    , monkeyToThrowTo: \i -> if i `mod` 13 == 0 then 1 else 3
    , itemsHandled: 0
    }
  , { items: 60:84:84:65:empty
    , operation: (_ + 7)
    , monkeyToThrowTo: \i -> if i `mod` 19 == 0 then 2 else 7
    , itemsHandled: 0
    } 
  , { items: 52:67:74:88:51:61:empty
    , operation: (_ * 3)
    , monkeyToThrowTo: \i -> if i `mod` 5 == 0 then 5 else 7
    , itemsHandled: 0
    }
  , { items: 67:72:empty
    , operation: (_ + 3)
    , monkeyToThrowTo: \i -> if i `mod` 2 == 0 then 1 else 2
    , itemsHandled: 0
    }
  , { items: 80:79:58:77:68:74:98:64:empty
    , operation: \i -> i * i
    , monkeyToThrowTo: \i -> if i `mod` 17 == 0 then 6 else 0
    , itemsHandled: 0
    }
  , { items: 62:53:61:89:86:empty
    , operation: (_ + 8)
    , monkeyToThrowTo: \i -> if i `mod` 11 == 0 then 4 else 6
    , itemsHandled: 0
    }
  , { items: 86:89:82:empty
    , operation: (_ + 2)
    , monkeyToThrowTo: \i -> if i `mod` 7 == 0 then 3 else 0
    , itemsHandled: 0
    }
  , { items: 92:81:70:96:69:84:83:empty
    , operation: (_ + 4)
    , monkeyToThrowTo: \i -> if i `mod` 3 == 0 then 4 else 5
    , itemsHandled: 0
    }
  ]

type MonkeyBusiness = State (Array Monkey)

throwItem :: WorryLevel -> MonkeyID -> MonkeyBusiness Unit
throwItem item id = state \monkeys -> unit /\ fromMaybe monkeys do
  monkeys # Array.alterAt id (\monkey -> Just $ monkey { items = CatQueue.snoc monkey.items item }) 

passItem :: MonkeyID -> MonkeyBusiness (Maybe WorryLevel)
passItem id = do
  monkeys <- State.get
  let 
    result = do
      monkey <- Array.index monkeys id
      item /\ items <- CatQueue.uncons monkey.items
      let newMonkey = monkey { items = items, itemsHandled = monkey.itemsHandled + 1 }
      state <- Array.alterAt id (\_ -> Just newMonkey) monkeys
      pure { item, monkey: newMonkey, state }

  case result of 
    Just {item, monkey, state} -> do
      State.put state
      let worry = monkey.operation item / 3
      throwItem worry (monkey.monkeyToThrowTo worry)
      pure $ Just worry 
    Nothing -> pure Nothing 
      
evaluateWhile :: ∀ s a. (s -> a -> Boolean) -> State s a -> State s a
evaluateWhile predicate st = do
  x <- st
  s <- State.get
  if predicate s x then evaluateWhile predicate st else pure x

evaluateMonkey :: MonkeyID -> MonkeyBusiness Unit
evaluateMonkey id = void $ evaluateWhile (const isJust) (passItem id)

evaluateRound :: MonkeyBusiness Unit
evaluateRound = do
  monkeys <- State.get
  (Array.range 0 $ Array.length monkeys - 1) # traverse_ evaluateMonkey

ans :: Array Monkey -> Int
ans inputs = finalState <#> _.itemsHandled # Array.sortBy (comparing negate) # Array.take 2 # product
  where
  finalState = State.execState run20Rounds inputs
  run20Rounds = runNRounds 20
  runNRounds n = Array.range 1 n # traverse_ (const evaluateRound)

run :: ∀ m. MonadEffect m => m Unit
run = logShow $ ans realInput 
