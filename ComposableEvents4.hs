
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

data Field a where
   OnInputText :: PlayerNumber -> Field String   -- A textbox will be created for the player. When filled, this event will fire and return the result
   OnInputCheckbox :: PlayerNumber -> Field Bool -- Idem with a checkbox
   OnInputButton :: PlayerNumber -> Field ()     -- Idem with a button
   OnTime :: UTCTime -> Field ()                 -- time event
   deriving Typeable

deriving instance Eq (Field a)
deriving instance Show (Field a)

data SomeField = forall a. Typeable a => SomeField (Field a)

data Event a where
   Sum :: Event a -> Event a -> Event a   -- The first event to fire will be returned
   Prod :: Event (a -> b) -> Event a -> Event b -- both events should fire, and then the result is returned
   Pure :: a -> Event a  -- Create a fake event. The result is useable with no delay.
   Empty :: Event a -- An event that is never fired. 
   Base :: (Typeable a) => Field a -> Event a
   deriving Typeable

instance Functor Event where
   fmap f a = pure f <*> a

instance Applicative Event where
   pure = Pure
   (<*>) = Prod

instance Alternative Event where
   (<|>) = Sum
   empty = Empty

data Nomex a where
   OnEvent :: Event a -> (a -> Nomex ()) -> Nomex ()
   Output :: String -> Nomex ()

data Game = Game { _events :: [EventHandler],
                   _outputs :: [String]}

data EventRes = forall e. Typeable e => EventRes 
       {bev :: Field e,
        res :: e}

deriving instance Typeable EventRes 

data EventHandler = forall e. EventHandler 
      { _event :: Event e,
        _handler :: e -> Nomex (),
        _env :: [EventRes]}

instance Show EventHandler where
   show (EventHandler _ _ en) = show $ length en

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
      (BE (Right _)) -> return $ EventHandler ev h env   

getRes :: SomeField -> IO (Maybe EventRes)
getRes (SomeField eb) = do
   mr <- getInputBase eb
   return $ EventRes eb <$> mr

getInputBase :: Field a -> IO (Maybe a)
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

getEventValue :: Event a -> [EventRes] -> BEither [SomeField] a
getEventValue (Pure a) _ = BE (Right a)
getEventValue Empty _ = BE (Left [])
getEventValue (Sum a b) ers = (getEventValue a ers) <|> (getEventValue b ers)
getEventValue (Prod f b) ers = (getEventValue f ers) <*> (getEventValue b ers)
getEventValue (Base a) ers = case (lookup' a ers) of
   Just r -> BE (Right r)
   Nothing -> BE (Left [SomeField a])

lookup' :: Typeable a => Field a -> [EventRes] -> Maybe a
lookup' _ [] = Nothing
lookup' be ((EventRes a r):ers) = case (cast (a,r)) of
   Just (a',r') -> if (a' == be) then Just r' else lookup' be ers
   _                      -> lookup' be ers


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

allPlayers = [1,2]

-- Now complex events can be created, such as voting systems:
voteEvent :: UTCTime -> Event ([Maybe Bool])
voteEvent time = sequenceA $ map (singleVote time) allPlayers
--voteEvent time = liftA [] <$> singleVote time 1 <*> singleVote time 2

singleVote :: UTCTime -> PlayerNumber -> Event (Maybe Bool)
singleVote timeLimit pn = (Just <$> onInputCheckbox pn) <|> (Nothing <$ onTime timeLimit)

vote :: UTCTime -> Event Bool
vote timeLimit = unanimity <$> (voteEvent timeLimit)

unanimity :: [Maybe Bool] -> Bool
unanimity = all (== Just True)

callVote :: UTCTime -> Nomex ()
callVote t = OnEvent (vote t) (Output . show)

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
   BE (Left  a)  <*>  BE (Left b)  =  BE (Left (a ++ b))
   BE (Left  e)  <*>  _  =  BE (Left e)
   BE (Right f)  <*>  r  =  fmap f r

instance Functor (BEither a) where
    fmap _ (BE (Left x))  = BE (Left x)
    fmap f (BE (Right y)) = BE (Right (f y))

main = do
   let g = Game [] []
   now <- getCurrentTime
   execStateT (evalNomex (callVote $ addUTCTime 10 now) >> forever runGame) g
   --execStateT (evalNomex (OnEvent (sequenceA [onInputMyAlternative, onInputMyAlternative]) (Output . show)) >> forever runGame) g

updateInputs :: StateT EventHandler IO ()
updateInputs = do
   eh <- get 
   eh' <- liftIO $ updateEH eh
   put eh'

triggerEvents :: EventHandler -> StateT Game IO ()
triggerEvents (EventHandler e h env) = do
   case getEventValue e env of
      BE (Right a) -> evalNomex $ h a
      BE (Left _) -> return ()

runGame :: StateT Game IO ()
runGame = do
   g <- get
   liftIO $ putStrLn $ "Output:" ++ (show $ _outputs g)
   liftIO $ putStrLn $ "Env:" ++ (show $ _events g)
   zoom (events . traverse) updateInputs
   mapM_ triggerEvents (_events g)

