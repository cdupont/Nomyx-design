--Solving the effect problem, using type parameter.
--a generic type 'r' is used to denote that an effectless instruction can be run in any context.
--Pro: it avoids to have to use a liftEffect (see TypeParamEffect-Concrete.hs)
--Cons: the type signatures are less elegant

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, DataKinds, ScopedTypeVariables, 
   MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad.State
import Control.Monad.Reader

data Effects = Effect | NoEffect

data Nomex :: Effects -> * -> * where
  ReadAccount  :: Nomex r Int              --ReadAccount has no effect: it can be run in whatever monad
  WriteAccount :: Int -> Nomex Effect ()   --WriteAccount has effect
  SetVictory   :: Nomex NoEffect Bool -> Nomex Effect () --SetVictory don't accept effectful computations
  Bind         :: Nomex m a -> (a -> Nomex m b) -> Nomex m b
  Return       :: a -> Nomex r a  --wrapping a constant has no effect

instance Monad (Nomex a) where
  return = Return
  (>>=) = Bind

moreMoney :: Nomex Effect ()
moreMoney = do
   a <- ReadAccount
   WriteAccount (a + 200)

winCondition :: Nomex NoEffect Bool
winCondition = do
   a <- ReadAccount
   --WriteAccount a
   return (a == 200)

data Game = Game { victory :: Nomex NoEffect Bool,
                   account :: Int}

evalEffect :: Nomex Effect a -> State Game a 
evalEffect ReadAccount      = liftEval ReadAccount
evalEffect (WriteAccount a) = modify (\g -> g{account = a})
evalEffect (SetVictory v)   = modify (\g -> g{victory = v})
evalEffect (Return a)       = liftEval (Return a)
evalEffect (Bind exp f)     = evalEffect exp >>= evalEffect . f

evalNoEffect :: Nomex NoEffect a -> Reader Game a
evalNoEffect ReadAccount  = asks account
evalNoEffect (Return a)   = return a
evalNoEffect (Bind exp f) = evalNoEffect exp >>= evalNoEffect . f

liftEval :: Nomex NoEffect a -> State Game a
liftEval r = do
   g <- get 
   return $ runReader (evalNoEffect r) g 

play :: Nomex Effect ()
play = do
   SetVictory $ winCondition
   moreMoney

isVictory :: Game -> Bool
isVictory g = runReader (evalNoEffect $ victory g) g

initGame = Game (return False) 0

main = putStrLn $ show $ isVictory $ execState (evalEffect play) initGame
