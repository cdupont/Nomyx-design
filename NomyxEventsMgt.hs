
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RankNTypes #-}

module NomyxEventsMgt where

import           Control.Applicative hiding (Const)
import           Control.Lens
import           Control.Monad.Error
import           Data.Typeable
import           GHC.Generics
import           Data.Time
import Control.Monad.State
import           Data.List
import           Data.Either
import           Data.Maybe
import           Safe
import           NomyxEvents



data Nomex a where
   --Events management
   OnEvent         :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> Nomex ()) -> Nomex EventNumber
   DelEvent        :: EventNumber -> Nomex Bool
   GetEvents       :: Nomex [EventInfoNomex]
   SendMessage     :: (Typeable a, Show a) => Msg a -> a -> Nomex ()

-- | The state of the game:
data Game = Game { _gameName    :: String,
                   _gEvents      :: [EventInfoNomex]}
                   deriving (Typeable)

type EvalNomex a = Evaluate Nomex Game a
type EventInfoNomex = EventInfo Nomex
type EvalEnvNomex = EvalEnv Nomex Game

-- | evaluate an effecful expression.
evalNomex :: Nomex a -> EvalNomex a
evalNomex (OnEvent ev h)          = evOnEvent ev h
evalNomex (DelEvent en)           = evDelEvent en
evalNomex (SendMessage m d)       = evSendMessage m d
evalNomex  GetEvents              = use events
--evalNomex (CurrentTime)           = undefined --use currentTime <$> asks _eGame


evOnEvent :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> Nomex ()) -> EvalNomex EventNumber
evOnEvent ev h = do
   evs <- use events
   let en = getFreeNumber (map _eventNumber evs)
   events %= (EventInfo en ev h SActive [] : )
   return en

evSendMessage :: (Typeable a, Show a) => Msg a -> a -> EvalNomex ()
evSendMessage m = triggerEvent (Message m)

evDelEvent :: EventNumber -> EvalNomex Bool
evDelEvent en = do
   evs <- use events
   case find ((== en) . getL eventNumber) evs of
      Nothing -> return False
      Just eh -> case _evStatus eh of
         SActive -> do
            events .= replaceWith ((== en) . getL eventNumber) eh{_evStatus = SDeleted} evs
            return True
         SDeleted -> return False

evTriggerTime :: UTCTime -> EvalNomex ()
evTriggerTime t = triggerEvent (Time t) t

--get the signals left to be completed in an event
getRemainingSignals :: EventInfoNomex -> Game -> [(SignalAddress, SomeSignal)]
getRemainingSignals (EventInfo _ e _ _ en) g = case runEvaluate g $ runEvalError (getEventResult e en) of
   Done _ -> []
   Todo a -> a

runEvaluate :: Game -> State EvalEnvNomex a -> a
runEvaluate game ev = evalState ev (EvalEnv (_gEvents game) game (void . evalNomex) errorHandlerNomex)

--extract the game state from an Evaluate
--knowing the rule number performing the evaluation (0 if by the system)
--and the player number to whom display errors (set to Nothing for all players)
--TODO: clean
runEvalError :: EvalNomex a -> State EvalEnvNomex a
runEvalError egs = undefined --modify (\g -> _eGame $ execState (runEvalError' mpn egs) (EvalEnv rn g evalNomex evalNomexNE))

runEvalError' :: EvalNomex a -> State EvalEnvNomex ()
runEvalError' egs = do
   e <- runErrorT egs
   case e of
      Right _ -> return ()
      Left e' -> undefined
         --tracePN (fromMaybe 0 mpn) $ "Error: " ++ e'
         --void $ runErrorT $ log mpn "Error: "

displayEvent :: Game -> EventInfoNomex -> String
displayEvent g ei@(EventInfo en _ _ s envi) =
   "event num: " ++ (show en) ++
   ", remaining signals: " ++ (show $ getRemainingSignals ei g) ++ --TODO: display also event result?
   ", envs: " ++ (show envi) ++
   ", status: " ++ (show s)

getFreeNumber :: (Eq a, Num a, Enum a) => [a] -> a
getFreeNumber l = head [ a | a <- [1..], notElem a l]

getL :: Getting a s a -> s -> a
getL = flip (^.)
replaceWith :: (a -> Bool)   -- ^ Value to search
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replaceWith f y = map (\z -> if f z then y else z)

errorHandlerNomex :: EventNumber -> String -> Evaluate n s ()
errorHandlerNomex en s = do
   --rn <- use eRuleNumber
   undefined
   --   logAll $ "Error in rule " ++ show rn ++ " (triggered by event " ++ show en ++ "): " ++ s
