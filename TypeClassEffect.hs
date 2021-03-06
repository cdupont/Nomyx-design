
--Solving the effect problem, using type classes.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad.State
import Control.Monad.Reader

class Monad m => NomexNoEffect m where
  readAccount :: m Int

class NomexNoEffect m => NomexEffect m where
  writeAccount :: Int -> m ()
  setVictory   :: (forall n. NomexNoEffect n => n Bool) -> m ()

data Game = Game { victory :: (forall m. NomexNoEffect m => m Bool)
                 , account :: Int
                 }

instance NomexNoEffect (State Game) where
  readAccount = gets account

instance NomexEffect (State Game) where
  writeAccount n = modify $ \game -> game { account = n }
  setVictory   v = modify $ \game -> game { victory = v }

instance NomexNoEffect (Reader Game) where
  readAccount = asks account

isVictory :: Game -> Bool
isVictory g = runReader (victory g) g

incrAccount :: NomexEffect m => m ()
incrAccount = readAccount >>= writeAccount . (+101)

winOnBigMoney :: NomexEffect m => m ()
winOnBigMoney = setVictory $ do
   i <- readAccount
   --writeAccount 100
   return (i > 100)

play = do
   winOnBigMoney
   incrAccount

initGame = Game (return False) 0

main = putStrLn $ show $ isVictory $ execState play initGame
