
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

--Complete example of a time limited vote using an Applicative/Alternative DSL.
--The design choice is to store temporary values from the inputs right inside the Event DSL.
--Pro: no need to manage references and a different list for the intermediate results.
--Con: The user get to see some internal (the intermediary value). How to solve that? Smart constructor?
module Main where

import Control.Applicative
import Data.Time
import Data.Traversable hiding (mapM)
import Data.Maybe
import Data.Typeable
import Control.Monad.State
import Safe
import Control.Lens
import Debug.Trace

type PlayerNumber = Int
type EventRef = Int

data BEvent a where
   OnInputText :: PlayerNumber -> BEvent String   -- A textbox will be created for the player. When filled, this event will fire and return the result
   OnInputCheckbox :: PlayerNumber -> BEvent Bool -- Idem with a checkbox
   OnInputButton :: PlayerNumber -> BEvent ()     -- Idem with a button
   OnTime :: UTCTime -> BEvent ()                 -- time event
   deriving Typeable

data Event a where
   Sum :: Event a -> Event a -> Event a   -- The first event to fire will be returned
   Prod :: Event (a -> b) -> Event a -> Event b -- both events should fire, and then the result is returned
   Pure :: a -> Event a  -- Create a fake event. The result is useable with no delay.
   Empty :: Event a -- An event that is never fired. 
   Base :: (Typeable a) => BEvent a -> Event a
   deriving Typeable

instance Functor Event where
   fmap f a = pure f <*> a

deriving instance Eq (BEvent a)

deriving instance Show (BEvent a)

instance Applicative Event where
   pure = Pure
   (<*>) = Prod

instance Alternative Event where
   (<|>) = Sum
   empty = Empty

onInputText = Base . OnInputText
onInputCheckbox = Base . OnInputCheckbox
onInputButton = Base . OnInputButton
onTime = Base . OnTime

-- A product type
data MyRecord = MyRecord String Bool deriving Show

-- A sum type
data MyAlternative = A | B deriving Show

-- Using the Applicative instance, we can build a product type from two separate event results.
-- The event callback should be called only when all two events have fired.
onInputMyRecord :: Event MyRecord
onInputMyRecord = MyRecord <$> onInputText 1 <*> onInputCheckbox 1

-- Using the Alternative instance, we build a sum type.
-- The event callback should be called when the first event have fired.
onInputMyAlternative :: Event MyAlternative
onInputMyAlternative = (A <$ onInputButton 1) <|> (B <$ onInputButton 1)

allPlayers = [1 .. 2]

-- Now complex events can be created, such as voting systems:
voteEvent :: UTCTime -> Event ([Maybe Bool])
voteEvent time = sequenceA $ map (singleVote time) allPlayers

singleVote :: UTCTime -> PlayerNumber -> Event (Maybe Bool)
singleVote timeLimit pn = (Just <$> onInputCheckbox pn) <|> (Nothing <$ onTime timeLimit)

vote :: UTCTime -> Event Bool
vote timeLimit = unanimity <$> (voteEvent timeLimit)

unanimity :: [Maybe Bool] -> Bool
unanimity = all (== Just True)

callVote :: UTCTime -> Nomex ()
callVote t = OnEvent (vote t) (Output . show)

data Nomex a where
   OnEvent :: Event a -> (a -> Nomex ()) -> Nomex ()
   Output :: String -> Nomex ()

data Game = Game { _events :: [EventHandler],
                   _outputs :: [String]}

data EventRes  where
    EventRes :: (Typeable e) => 
       {bev :: BEvent e,
        res :: e} -> EventRes

deriving instance Typeable EventRes 


data EventHandler where 
   EventHandler :: 
      { _event :: Event e,
        _handler :: e -> Nomex (),
        _env :: [EventRes]} -> EventHandler

makeLenses ''Game
makeLenses ''EventHandler

--Evaluation
evalNomex :: Nomex a -> StateT Game IO a
evalNomex (OnEvent e h) = events %= (EventHandler e h []:)
evalNomex (Output s) = outputs %= (s:)

updateEH :: EventHandler -> IO (EventHandler)
updateEH (EventHandler ev h env) = do
   case (getEventValue ev env) of
      (BE (Left ebs)) -> do
         rs <- mapM getRes ebs
         return (EventHandler ev h (env ++ (catMaybes rs)))
      (BE (Right _)) -> return (EventHandler ev h env)   

getRes :: SomeBEvent -> IO (Maybe EventRes)
getRes (SomeBEvent eb) = do
   mr <- getInputBase eb
   return $ EventRes eb <$> mr

updateInput :: (Typeable b) => Event a -> (BEvent b, b) -> Event a
updateInput (Base a) r = case (cast r) of
   Just (be, b) | a == be -> Pure b
   _                      -> Base a
updateInput (Sum e1 e2) r = Sum (updateInput e1 r) (updateInput e2 r)
updateInput (Prod f e) r = Prod (updateInput f r) (updateInput e r)
updateInput (Pure e) _ = Pure e
updateInput (Empty) _ = Empty

-- | an equality that tests also the types.
(===) :: (Typeable a, Typeable b, Eq b) => a -> b -> Bool
(===) x y = cast x == Just y

getInputBase :: BEvent a -> IO (Maybe a)
getInputBase (OnInputText pn) = do
   putStrLn $ "Player " ++ (show pn) ++ ": enter text"
   Just <$> getLine
getInputBase (OnInputButton pn) = do
   putStrLn $ "Player " ++ (show pn) ++ ": press b"
   s <- getChar
   return $ if s == 'b' then Just () else Nothing
getInputBase (OnInputCheckbox pn) = do
   putStrLn $ "Player " ++ (show pn) ++ ": enter True/False"
   s <- getLine
   return $ readMay s
getInputBase (OnTime t) = do
   now <- getCurrentTime
   return $ if now > t then Just () else Nothing

data SomeBEvent where
    SomeBEvent :: (Typeable e) => {unEv :: BEvent e} -> SomeBEvent

--fromSomeBEvent :: SomeBEvent -> BEvent a
--fromSomeBEvent (SomeBEvent e) = e

deriving instance Show SomeBEvent

newtype BEither a b = BE (Either a b) deriving (Show, Eq, Typeable)
bLeft = BE . Left
bRight = BE . Right


instance Alternative (BEither [a]) where
   empty        = bLeft []
   BE (Left a) <|> BE (Left b) = bLeft $ a ++ b
   BE (Left _) <|> n = n
   m      <|> _ = m

instance Applicative (BEither [a]) where
   pure = BE . Right 
   BE (Left  f)  <*>  BE (Left b)  =  BE (Left (f ++ b))
   BE (Left  e)  <*>  _  =  BE (Left e)
   BE (Right f)  <*>  r  =  fmap f r

instance Functor (BEither a) where
    fmap _ (BE (Left x))  = BE (Left x)
    fmap f (BE (Right y)) = BE (Right (f y))

getEventValue :: Event a -> [EventRes] -> BEither [SomeBEvent] a
getEventValue (Pure a) _ = BE (Right a)
getEventValue Empty _ = BE (Left [])
getEventValue (Sum a b) ers = (getEventValue a ers) <|> (getEventValue b ers)
getEventValue (Prod f b) ers = (getEventValue f ers) <*> (getEventValue b ers)
getEventValue (Base a) ers = case (lookup' a ers) of
   Just r -> BE (Right r)
   Nothing -> BE (Left [SomeBEvent a])

lookup' :: Typeable a => BEvent a -> [EventRes] -> Maybe a
lookup' _ [] = Nothing
lookup' be ((EventRes a r):ers) = case (cast (a,r)) of
   Just (a',r') -> if (a' == be) then Just r' else Nothing
   _                      -> lookup' be ers


main = undefined


