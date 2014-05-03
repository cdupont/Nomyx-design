
{-# LANGUAGE GADTs #-}

module ComposableEvents where

import Control.Applicative
import Data.Time
import Data.Traversable

--data EventState e = EventState {event :: Event e, 
--                              callback :: EventData e -> IO ()}

type PlayerNumber = Int

data Event a where
   OnInputText :: PlayerNumber -> Event String   -- A textbox will be created for the player. When filled, this event will fire and return the result
   OnInputCheckbox :: PlayerNumber -> Event Bool -- Idem with a checkbox
   OnInputButton :: PlayerNumber -> Event ()     -- Idem with a button
   OnTime :: UTCTime -> Event ()                 -- time event
   EventSum :: Event a -> Event a -> Event a   -- The first event to fire will be returned
   EventProduct :: Event (a -> b) -> Event a -> Event b  -- both events should fire, and then the result is returned
   Fmap :: (a -> b) -> Event a -> Event b  -- transforms the value returned by an event.
   Pure :: a -> Event a  -- Create a fake event. The result is useable with no delay.
   Empty :: Event a -- An event that is never fired. 

instance Functor Event where
   fmap = Fmap

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
data MyRecord = MyRecord String Bool

-- A sum type
data MyAlternative = A | B

-- Using the Applicative instance, we can build a product type from two separate event results.
-- The event callback should be called only when all two events have fired.
onInputMyRecord :: Event MyRecord
onInputMyRecord = MyRecord <$> onInputText 1 <*> onInputCheckbox 1

-- other possible implementation (given a monad instance)
-- onInputMyRecord' = do
--    s <- onInputText
--    b <- onInputCheckbox
--    return $ MyRecord s b

-- Using the Alternative instance, we build a sum type.
-- The event callback should be called when the first event have fired.
onInputMyAlternative :: Event MyAlternative
onInputMyAlternative = (const A <$> onInputButton 1) <|> (const B <$> onInputButton 1)

allPlayers = [1 .. 10]

-- Now complex events can be created, such as voting systems:
voteEvent :: UTCTime -> Event ([Maybe Bool])
voteEvent time = sequenceA $ map (singleVote time) allPlayers

singleVote :: UTCTime -> PlayerNumber -> Event (Maybe Bool)
singleVote timeLimit pn = (Just <$> onInputCheckbox pn) <|> (const Nothing <$> onTime timeLimit)

vote :: UTCTime -> Event Bool
vote timeLimit = unanimity <$> (voteEvent timeLimit)

unanimity :: [Maybe Bool] -> Bool
unanimity = all (== Just True)


--Evaluation
--evalEvent :: Event a -> State Game a
--evalEvent = undefined

