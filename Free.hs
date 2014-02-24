module Main where

import Control.Monad.Free (iterM, liftF, Free)
import Control.Monad (join)

data NoEffectF a = ReadAccount (Int -> a) | SetVictory (NoEffect Bool) a
data EffectF a = NoEffect (NoEffectF a) | WriteAccount Int a

instance Functor NoEffectF where
  fmap f (ReadAccount g) = ReadAccount (f . g)
  fmap f (SetVictory cond x) = SetVictory cond (f x)

instance Functor EffectF where
  fmap f (NoEffect x) = NoEffect (fmap f x)
  fmap f (WriteAccount amount x) = WriteAccount amount (f x)

type NoEffect a = Free NoEffectF a
type Effect a = Free EffectF a

liftNoEffect :: NoEffect a -> Effect a
liftNoEffect = iterM (join . liftF . NoEffect)

readAccount :: NoEffect Int
readAccount = liftF (ReadAccount id)

writeAccount :: Int -> Effect ()
writeAccount a = liftF (WriteAccount a ())

setVictory :: NoEffect Bool -> NoEffect ()
setVictory cond = liftF (SetVictory cond ())

moreMoney :: Effect ()
moreMoney = do
  a <- liftNoEffect readAccount
  writeAccount (a + 200)

winCondition :: NoEffect Bool
winCondition = do
  a <- readAccount
  return (a == 200)

play :: Effect ()
play = do
  liftNoEffect (setVictory winCondition)
  moreMoney

main = undefined
