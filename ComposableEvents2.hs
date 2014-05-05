
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

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

instance Functor Event where
   fmap f a = pure f <*> a

instance Applicative Event where
   pure = Pure
   (<*>) = EventProduct

instance Alternative Event where
   (<|>) = EventSum
   empty = Empty

--instance Traversable Event where
--   traverse f Empty = pure Empty
--   traverse f (Pure x) = Pure <$> f x
--   traverse f (EventSum a b) = f a <|> f b

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

-- other possible implementation (given a monad instance)
-- onInputMyRecord' = do
--    s <- onInputText
--    b <- onInputCheckbox
--    return $ MyRecord s b

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

callVote :: UTCTime -> Nomex ()
callVote t = OnEvent (vote t) (Output . show)

data Nomex a where
   OnEvent :: Event a -> (a -> Nomex ()) -> Nomex ()
   Output :: String -> Nomex ()


data Game = Game { _events :: [EventHandler],
                   _outputs :: [String]}

data EventHandler where 
   EventHandler :: 
      { _event :: Event e,
        _handler :: e -> Nomex ()} -> EventHandler

makeLenses ''Game
makeLenses ''EventHandler

--Evaluation
evalNomex :: Nomex a -> StateT Game IO a
evalNomex (OnEvent e h) = events %= (EventHandler e h :)
evalNomex (Output s) = outputs %= (s:)
   
updateInput :: Event a -> IO (Event a)
updateInput (OnInputText pn) = do
   putStrLn $ "Player " ++ (show pn) ++ ": enter text"
   s <- getLine
   return $ pure s
updateInput i@(OnInputButton pn) = do
   putStrLn $ "Player " ++ (show pn) ++ ": press b"
   s <- getChar
   return $ if s == 'b' then pure () else i
updateInput i@(OnInputCheckbox pn) = do
   putStrLn $ "Player " ++ (show pn) ++ ": enter True/False"
   s <- getLine
   return $ case (readMay s) of
      Just b -> pure b
      Nothing -> i
updateInput i@(OnTime t) = do
   now <- getCurrentTime
   return $ if now > t then pure () else i
updateInput (EventSum a b) = do
   na <- (updateInput a)
   nb <- (updateInput b)
   return $ na <|> nb
updateInput (EventProduct a b) = do
   na <- (updateInput a) 
   nb <- (updateInput b)
   return $ na <*> nb
updateInput a = return a

getEventValue :: Event a -> Maybe a 
getEventValue (Pure a) = Just a
getEventValue Empty = Nothing
getEventValue (EventSum a b) = (getEventValue a) <|> (getEventValue b)
getEventValue (EventProduct a b) = (getEventValue a) <*> (getEventValue b)
getEventValue _ = Nothing

updateInputs :: StateT EventHandler IO ()
updateInputs = do 
   (EventHandler e h) <- get
   e' <- liftIO $ updateInput e
   put $ EventHandler e' h

triggerEvents :: EventHandler -> StateT Game IO ()
triggerEvents (EventHandler e h) = do
   let mval = getEventValue e
   when (isJust mval) $ do
      evalNomex $ h $ fromJust mval
      events .= []

runGame :: StateT Game IO ()
runGame = do
   g <- get
   liftIO $ putStrLn $ show $ _outputs g
   zoom (events . traverse) updateInputs
   mapM_ triggerEvents (_events g)

play :: Nomex ()
play = OnEvent onInputMyRecord (Output . show)

main = do
   let g = Game [] []
   now <- getCurrentTime
   execStateT (evalNomex (callVote $ addUTCTime 5 now) >> forever runGame) g
   
