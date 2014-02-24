--Solving the effect problem, using type parameter.
--The effectless instructions are marked with 'NoEffect'. To run them in an effect-full context,
--we are obliged to `liftNoEffect` them.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, DataKinds, ScopedTypeVariables, 
   MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad.State
import Control.Monad.Reader

data Effects = Effect | NoEffect

data Nomex :: Effects -> * -> * where
  ReadAccount  :: Nomex NoEffect Int                          --ReadAccount has no effect
  WriteAccount :: Int -> Nomex Effect ()                      --WriteAccount has effect
  SetVictory   :: Nomex NoEffect Bool -> Nomex Effect ()      --SetVictory don't accept effectful computations
  Bind         :: Nomex m a -> (a -> Nomex m b) -> Nomex m b
  Return       :: a -> Nomex r a                              
  NoEff        :: Nomex NoEffect a -> Nomex Effect a          --Wrapping an effect-less instruction into an effect-full one

instance Monad (Nomex a) where
  return = Return
  (>>=) = Bind

moreMoney :: Nomex Effect ()
moreMoney = do
   a <- liftNoEffect ReadAccount   
   WriteAccount (a + 200)

winCondition :: Nomex NoEffect Bool
winCondition = do
   a <- ReadAccount
   --WriteAccount a
   return (a == 200)

data Game = Game { victory :: Nomex NoEffect Bool,
                   account :: Int}

evalEffect :: Nomex Effect a -> State Game a 
evalEffect (WriteAccount a)   = modify (\g -> g{account = a})
evalEffect (SetVictory v)     = modify (\g -> g{victory = v})
evalEffect (Return a)         = liftEval $ evalNoEffect (Return a)
evalEffect (Bind exp f)       = evalEffect exp >>= evalEffect . f
evalEffect (NoEff a)          = liftEval $ evalNoEffect a

evalNoEffect :: Nomex NoEffect a -> Reader Game a
evalNoEffect ReadAccount = asks account
evalNoEffect (Return a) = return a
evalNoEffect (Bind exp f) = evalNoEffect exp >>= evalNoEffect . f

liftEval :: Reader Game a -> State Game a
liftEval r = get >>= return . runReader r 

liftNoEffect :: Nomex NoEffect a -> Nomex Effect a
liftNoEffect = NoEff 

play :: Nomex Effect ()
play = do
   SetVictory $ winCondition
   moreMoney

isVictory :: Game -> Bool
isVictory g = runReader (evalNoEffect $ victory g) g

initGame = Game (return False) 0

main = putStrLn $ show $ isVictory $ execState (evalEffect play) initGame
