
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RankNTypes #-}

module NomyxEvents where

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

type EventNumber = Int

-- | Composable events
data Event a where
   SumEvent       :: Event a -> Event a -> Event a                        -- The first event to fire will be returned
   AppEvent       :: Event (a -> b) -> Event a -> Event b                 -- Both events should fire, and then the result is returned
   PureEvent      :: a -> Event a                                         -- Create a fake event. The result is useable with no delay.
   EmptyEvent     :: Event a                                              -- An event that is never fired.
   BindEvent      :: Event a -> (a -> Event b) -> Event b                 -- A First event should fire, then a second event is constructed
   ShortcutEvents :: [Event a] -> ([Maybe a] -> Bool) -> Event [Maybe a]  -- Return the intermediate results as soon as the function evaluates to True, dismissing the events that hasn't fired yet
   SignalEvent    :: (Typeable a) => Signal a -> Event a                  -- Embed a single Signal as an Event
   --LiftEvent      :: Nomex a -> Event a                                 -- create an event containing the result of the NomexNE.
   deriving Typeable

-- | Signals
-- A signal is something that may occur at a point in time.
-- They are the leafs of the event tree
data Signal a where
   --Player  :: Player    -> Signal PlayerInfo                       -- Fires on events related to players.
   --Input   :: String -> (InputForm a) -> Signal a  -- Fires when the user has complete the input form. Input forms are created automatically when the event is posted.
   Time    :: UTCTime   -> Signal UTCTime                          -- Fires at the specified date.
   Message :: Msg a     -> Signal a                                -- Fires if a message is received.
   --Custom  :: a         -> Signal b                                --
   deriving Typeable

-- | Events parameters
data Msg m     = Msg String deriving (Typeable, Show)

-- | Type agnostic result data
data SomeData = forall e. (Typeable e, Show e) => SomeData e
deriving instance Show SomeData

deriving instance Eq (Signal e)
deriving instance Eq (Msg e)
deriving instance Show (Signal a)
deriving instance Show SomeSignal

instance Functor Event where
   fmap f a = pure f <*> a

instance Applicative Event where
   pure = PureEvent
   (<*>) = AppEvent

instance Alternative Event where
   (<|>) = SumEvent
   empty = EmptyEvent

instance Monad Event where
   (>>=) = BindEvent
   return = PureEvent

instance MonadPlus Event where
   mplus = SumEvent
   mzero = EmptyEvent


-- * EventInfo

-- EventInfo holds all infos on a active event
data EventInfo n = forall a. (Typeable a, Show a) =>
   EventInfo {_eventNumber :: EventNumber,
              event        :: Event a,
              handler      :: (EventNumber, a) -> n (),
              _evStatus    :: Status,
              _env         :: [SignalOccurence]}


data EvalEnv n s = EvalEnv { _events      :: [EventInfo n],
                             _execState   :: s,
                             evalFunc     :: forall a. (Show a) => n a -> Evaluate n s (),       -- evaluation function
                             errorHandler :: EventNumber -> String -> Evaluate n s ()}

-- | Environment necessary for the evaluation of Nomex
type Evaluate n s a = ErrorT String (State (EvalEnv n s)) a


-- SignalAddress is a representation of the address of a signal in the event tree
type SignalAddress = [SignalAddressElem]
data SignalAddressElem = SumR | SumL | AppR | AppL | BindR | BindL | Shortcut deriving (Show, Read, Ord, Eq, Generic)

-- result data from a signal
data SignalData = forall e. (Typeable e, Show e) =>
   SignalData {signal     :: Signal e,
               signalData :: e}

-- data and addres from an occurence of a signal
data SignalOccurence = SignalOccurence {_signalOccData    :: SignalData,
                                        _signalOccAddress :: SignalAddress}

-- | Type agnostic base signal
data SomeSignal = forall a. (Typeable a) => SomeSignal (Signal a)

deriving instance Show SignalData
deriving instance Show SignalOccurence

-- status of an event
data Status = SActive | SDeleted deriving (Eq, Show)

instance Eq (EventInfo n) where
   (EventInfo {_eventNumber=e1}) == (EventInfo {_eventNumber=e2}) = e1 == e2

instance Ord (EventInfo n) where
   (EventInfo {_eventNumber=e1}) <= (EventInfo {_eventNumber=e2}) = e1 <= e2

-- * Events management

--class (Monad m, Functor m, Typeable m) => EvMgt m where
--   onEvent         :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> m ()) -> m EventNumber
--   delEvent        :: EventNumber -> m Bool
--   getEvents       :: m [EventInfo]
--   sendMessage     :: (Typeable a, Show a) => Msg a -> a -> m ()
--   currentTime     :: m UTCTime

makeLenses ''SignalOccurence
makeLenses ''EvalEnv
makeLenses ''EventInfo

eventNumber :: Lens' (EventInfo n) EventNumber
eventNumber f (EventInfo e ev h evs env) = fmap (\e' -> (EventInfo e' ev h evs env)) (f e)

evStatusNumber :: Lens' (EventInfo n) Status
evStatusNumber f (EventInfo e ev h evs env) = fmap (\evs' -> (EventInfo e ev h evs' env)) (f evs)

env :: Lens' (EventInfo n) [SignalOccurence]
env f (EventInfo e ev h evs env) = fmap (\env' -> (EventInfo e ev h evs env')) (f env)

-- trigger an event with an event result
triggerEvent :: (Typeable e, Show e) => Signal e -> e -> Evaluate n s ()
triggerEvent s dat = do
   evs <- gets _events
   triggerEvent' (SignalData s dat) Nothing evs

-- trigger some specific signal
triggerEvent' :: SignalData -> Maybe SignalAddress -> [EventInfo n] -> Evaluate n s ()
triggerEvent' sd msa evs = do
   let evs' = evs -- sortBy (compare `on` _ruleNumber) evs
   eids <- mapM (getUpdatedEventInfo sd msa) evs'           -- get all the EventInfos updated with the field
   events %= union (map fst eids)                           -- store them
   void $ mapM triggerIfComplete eids                           -- trigger the handlers for completed events

-- if the event is complete, trigger its handler
triggerIfComplete :: (EventInfo n, Maybe SomeData) -> Evaluate n s ()
triggerIfComplete (EventInfo en _ h SActive _, Just (SomeData val)) = case cast val of
   Just a -> do
        eval <- gets evalFunc
        err <- gets errorHandler
        (void $ (eval $ h (en, a))) `catchError` (err en)
   Nothing -> error "Bad trigger data type"
triggerIfComplete _ = return ()


-- get update the EventInfo updated with the signal data.
-- get the event result if all signals are completed
getUpdatedEventInfo :: SignalData -> Maybe SignalAddress -> EventInfo n -> Evaluate n s (EventInfo n, Maybe SomeData)
getUpdatedEventInfo sd@(SignalData sig _) addr ei@(EventInfo _ ev _ _ envi) = do
   trs <- getEventResult ev envi
   case trs of
      Todo rs -> case find (\(sa, ss) -> (ss == SomeSignal sig) && maybe True (==sa) addr) rs of -- check if our signal match one of the remaining signals
         Just (sa, _) -> do
            let envi' = SignalOccurence sd sa : envi
            er <- getEventResult ev envi'                                                           -- add our event to the environment and get the result
            return $ case er of
               Todo _ -> (env .~ envi' $ ei, Nothing)                                              -- some other signals are left to complete: add ours in the environment
               Done a -> (env .~  [] $ ei, Just $ SomeData a)                                       -- event complete: return the final data result
         Nothing -> return (ei, Nothing)                                                            -- our signal does not belong to this event.
      Done a -> return (env .~  [] $ ei, Just $ SomeData a)

--get the signals left to be completed in an event
getRemainingSignals' :: EventInfo n -> Evaluate n s [(SignalAddress, SomeSignal)]
getRemainingSignals' (EventInfo _ e _ _ envi) = do
   tr <- getEventResult e envi
   return $ case tr of
      Done _ -> []
      Todo a -> a

-- compute the result of an event given an environment.
-- in the case the event cannot be computed because some signals results are pending, return that list instead.
getEventResult :: Event a -> [SignalOccurence] -> Evaluate n s (Todo (SignalAddress, SomeSignal) a)
getEventResult e frs = getEventResult' e frs []

-- compute the result of an event given an environment. The third argument is used to know where we are in the event tree.
getEventResult' :: Event a -> [SignalOccurence] -> SignalAddress -> Evaluate n s (Todo (SignalAddress, SomeSignal) a)
getEventResult' (PureEvent a)   _   _  = return $ Done a
getEventResult'  EmptyEvent     _   _  = return $ Todo []
getEventResult' (SumEvent a b)  ers fa = liftM2 (<|>) (getEventResult' a ers (fa ++ [SumL])) (getEventResult' b ers (fa ++ [SumR]))
getEventResult' (AppEvent f b)  ers fa = liftM2 (<*>) (getEventResult' f ers (fa ++ [AppL])) (getEventResult' b ers (fa ++ [AppR]))
--getEventResult' (LiftEvent a)   _   _  = do
--   evalNomexNE <- asks evalNomexNEFunc
--   r <- evalNomexNE a
--   return $ Done r
getEventResult' (BindEvent a f) ers fa = do
   er <- getEventResult' a ers (fa ++ [BindL])
   case er of
      Done a' -> getEventResult' (f a') ers (fa ++ [BindR])
      Todo bs -> return $ Todo bs

getEventResult' (SignalEvent a)  ers fa = return $ case lookupSignal a fa ers of
   Just r  -> Done r
   Nothing -> Todo [(fa, SomeSignal a)]

getEventResult' (ShortcutEvents es f) ers fa = do
  (ers' :: [Todo (SignalAddress, SomeSignal) a]) <- mapM (\e -> getEventResult' e ers (fa ++ [Shortcut])) es -- get the result for each event in the list
  return $ if f (toMaybe <$> ers')                                                                     -- apply f to the event results that we already have
     then Done $ toMaybe <$> ers'                                                                        -- if the result is true, we are done. Return the list of maybe results
     else Todo $ join $ lefts $ toEither <$> ers'                                                        -- otherwise, return the list of remaining fields to complete from each event





-- A todo list of things left to be done before obtaining a result.
data Todo a b = Todo [a] | Done b
   deriving (Eq, Ord, Read, Show, Typeable)

instance Alternative (Todo a) where
   empty               = Todo []
   Todo as <|> Todo bs = Todo $ as ++ bs
   Todo _  <|> n       = n
   m       <|> _       = m

instance Applicative (Todo a) where
   pure                = Done
   Todo as <*> Todo bs = Todo $ as ++ bs
   Todo as <*> _       = Todo as
   Done f  <*> r       = fmap f r

instance Functor (Todo a) where
   fmap _ (Todo x) = Todo x
   fmap f (Done y) = Done $ f y

instance Monad (Todo a) where
   return = Done
   Todo as >>= _ = Todo as
   Done a >>= f = f a

toEither :: Todo a b -> Either [a] b
toEither (Todo as) = Left as
toEither (Done a)  = Right a

fromEither :: Either [a] b -> Todo a b
fromEither (Left as) = Todo as
fromEither (Right a) = Done a

toMaybe :: Todo a b -> Maybe b
toMaybe (Todo _) = Nothing
toMaybe (Done a) = Just a


-- find a signal occurence in an environment
lookupSignal :: Typeable a => Signal a -> SignalAddress -> [SignalOccurence] -> Maybe a
lookupSignal s sa envi = headMay $ mapMaybe (getSignalData s sa) envi

--get the signal data from the signal occurence
getSignalData :: Typeable a => Signal a -> SignalAddress -> SignalOccurence -> Maybe a
getSignalData s sa (SignalOccurence (SignalData s' res) sa') = do
   ((s'', res') :: (Signal a, a)) <- cast (s', res)
   if (s'' == s) && (sa' == sa) then Just res' else Nothing


--errorHandler :: EventNumber -> String -> Evaluate n s ()
--errorHandler en s = do
   --rn <- use eRuleNumber
--   undefined
   --   logAll $ "Error in rule " ++ show rn ++ " (triggered by event " ++ show en ++ "): " ++ s

instance Eq SomeSignal where
  (SomeSignal e1) == (SomeSignal e2) = e1 === e2

-- | an equality that tests also the types.
(===) :: (Typeable a, Typeable b, Eq b) => a -> b -> Bool
(===) x y = cast x == Just y
