
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

-- A DSL to represent complex events.
-- intermediate results are not stored, so all the input must be consumed.
module Main where

import Control.Applicative
import Data.Time
import Data.Traversable hiding (mapM)
import Data.Maybe
import Data.Typeable
import Control.Monad.State
import Safe

type PlayerNumber = Int
type EventRef = Int

data Event a where
   OnInputText :: PlayerNumber -> Event String   -- A textbox will be created for the player. When filled, this event will fire and return the result
   OnInputCheckbox :: PlayerNumber -> Event Bool -- Idem with a checkbox
   OnInputButton :: PlayerNumber -> Event ()     -- Idem with a button
   OnTime :: UTCTime -> Event ()                 -- time event
   EventSum :: Event a -> Event a -> Event a   -- The first event to fire will be returned
   EventProduct :: Event (a -> b) -> Event a -> Event b  -- both events should fire, and then the result is returned
   Pure :: a -> Event a  -- Create a fake event. The result is useable with no delay.
   Empty :: Event a -- An event that is never fired. 
   Ref :: EventRef -> Event a -> Event a

instance Functor Event where
   fmap f a = pure f <*> a

instance Applicative Event where
   pure = Pure
   (<*>) = EventProduct

instance Alternative Event where
   (<|>) = EventSum
   empty = Empty

onInputText = OnInputText
onInputCheckbox = OnInputCheckbox
onInputButton = OnInputButton
onTime = OnTime

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
onInputMyAlternative = (const A <$> onInputButton 1) <|> (const B <$> onInputButton 1)

allPlayers = [1 .. 2]

-- Now complex events can be created, such as voting systems:
voteEvent :: UTCTime -> Event ([Maybe Bool])
voteEvent time = sequenceA $ map (singleVote time) allPlayers

singleVote :: UTCTime -> PlayerNumber -> Event (Maybe Bool)
singleVote timeLimit pn = (Just <$> onInputCheckbox pn) <|> (const Nothing <$> onTime timeLimit)

vote :: UTCTime -> Event Bool
vote timeLimit = unanimity <$> (voteEvent timeLimit)

unanimity :: [Maybe Bool] -> Bool
unanimity = all (== Just True)

callVote :: Nomex ()
callVote = OnEvent (vote $ read "0") (Output . show)

data Nomex a where
   OnEvent :: Event a -> (a -> Nomex ()) -> Nomex ()
   Output :: String -> Nomex ()


data Game = Game { events :: [EventHandler],
                   inputResults :: Typeable a => [(EventRef, Maybe a)],
                   outputs :: [String]}

type EventNumber = Int

data EventHandler where 
   EventHandler :: 
      { --eventNumber :: EventNumber,
        event :: Event e,
        handler :: e -> Nomex ()} -> EventHandler

data Environment i 
   = Environment { inputs :: [(EventRef, Maybe i)]}

--Evaluation
evalNomex :: Nomex a -> StateT Game IO a
evalNomex (OnEvent e h) = do
   g <- get
   put $ g { events = EventHandler e h : (events g)}
evalNomex (Output s) = do
   g <- get
   put $ g { outputs = s : (outputs g)}
   
getInput :: Event a -> IO (Maybe a)
getInput (OnInputText pn) = do
   putStrLn $ "Player " ++ (show pn) ++ ": enter text"
   s <- getLine
   return $ Just s
getInput (OnInputButton pn) = do
   putStrLn $ "Player " ++ (show pn) ++ ": press b"
   s <- getChar
   return $ if s == 'b' then Just () else Nothing
getInput (OnInputCheckbox pn) = do
   putStrLn $ "Player " ++ (show pn) ++ ": enter True/False"
   s <- getLine
   return $ readMay s
getInput (EventSum a b) = (getInput a) `mplus` (getInput b)
getInput (EventProduct a b) = do
   f <- getInput a
   b <- getInput b
   return $ f <*> b 
getInput (Pure a) = return $ Just a
getInput Empty = return $ Nothing

triggerEvents :: EventHandler -> StateT Game IO ()
triggerEvents (EventHandler e h) = do
   mr <- liftIO $ getInput e
   when (isJust mr) $ evalNomex $ h $ fromJust mr

runGame :: StateT Game IO ()
runGame = do
   g <- get
   liftIO $ putStrLn $ show $ outputs g
   mapM_ triggerEvents (events g)

play :: Nomex ()
play = do
   OnEvent onInputMyRecord (Output . show)

main = do
   let g = Game [] [] []
   execStateT (evalNomex play >> forever runGame) g
   
