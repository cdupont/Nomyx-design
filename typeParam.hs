
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, DataKinds, ScopedTypeVariables, 
   MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module DSLEffects where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Typeable

data Effects = Effect | NoEffect

data Nomex :: Effects -> * -> * where
  ReadAccount  :: Nomex r Int            --ReadAccount has no effect: it can be run in whatever monad
  WriteAccount :: Int -> Nomex Effect ()  --WriteAccount has effect
  SetVictory   :: Nomex NoEffect Bool -> Nomex Effect () --SetVictory don't accept effectful computations
  Bind         :: Nomex m a -> (a -> Nomex m b) -> Nomex m b
  Return       :: a -> Nomex r a  --wrapping a constant has no effect

instance Monad (Nomex a) where
  return = Return
  (>>=) = Bind


noEff :: Nomex NoEffect ()
noEff = return ()

hasEffect :: Nomex Effect ()
hasEffect = do
   a <- ReadAccount
   WriteAccount a

data Game = Game { victory :: Nomex NoEffect Bool,
                   account :: Int}

evalEffect :: Nomex Effect a -> State Game a 
evalEffect a@ReadAccount     = liftEval $ evalNoEffect a
evalEffect (WriteAccount a)   = modify (\g -> g{account = a})
evalEffect (SetVictory v)     = modify (\g -> g{victory = v})
evalEffect a@(Return _)       = liftEval $ evalNoEffect a
evalEffect (Bind exp f)       = evalEffect exp >>= evalEffect . f

evalNoEffect :: Nomex r a -> Reader Game a
evalNoEffect ReadAccount = asks account
evalNoEffect (Return a) = return a
evalNoEffect (Bind exp f) = evalNoEffect exp >>= evalNoEffect . f

liftEval :: Reader Game a -> State Game a
liftEval r = get >>= return . runReader r 

