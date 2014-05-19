
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

--import Control.Lens.Setter
--import Control.Monad.Trans.Error

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
  -- Prod' :: Event a -> Event b -> Event (a,b)   
--Bind :: Event a -> (a -> Event b) -> Event b
   Pure :: a -> Event a  -- Create a fake event. The result is useable with no delay.
   Empty :: Event a -- An event that is never fired. 
   Base :: (Typeable a) => BEvent a -> Event a
   deriving Typeable

instance Functor Event where
   fmap f a = pure f <*> a

deriving instance Eq (BEvent a)

deriving instance Show (BEvent a)
--instance (Eq a) => Eq (Event a) where
--   (OnInputText pn1) == (OnInputText pn2) = pn1 == pn2 
--   (OnInputCheckbox pn1) == (OnInputCheckbox pn2) = pn1 == pn2
--   (OnInputButton pn1) == (OnInputButton pn2) = pn1 == pn2
--   (OnTime t1) == (OnTime t2) = t1 == t2 
--   (Sum a b) == (Sum c d) = (a,b) == (c,d)
--   (Map f e) == (Map g h) = False
--   (Pure a) == (Pure b) = a == b
--   Empty == Empty = True

--instance Functor Event where
--   fmap = Map

instance Applicative Event where
   pure = Pure
   (<*>) = Prod

instance Alternative Event where
   (<|>) = Sum
   empty = Empty

--instance Monad Event where
--   (>>=) = Bind
--  return = pure
--instance Traversable Event where
--   traverse f Empty = pure Empty
--   traverse f (Pure x) = Pure <$> f x
--   traverse f (EventSum a b) = f a <|> f b

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

-- other possible implementation (given a monad instance)
-- onInputMyRecord' = do
--    s <- onInputText
--    b <- onInputCheckbox
--    return $ MyRecord s b

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
   
--getProductEvents :: Event a -> [Event b]
--getProductEvents i@(OnInputText _) = [i]
--getProductEvents i@(OnInputButton _) = [i]
--getProductEvents i@(OnInputCheckbox _) = [i]
--getProductEvents i@(OnTime _) = []
--getProductEvents i@(EventProduct a b) = [a, b]
--getProductEvents i@(EventProduct a b) = [a, b]


updateInput :: (Typeable b) => Event a -> (BEvent b, b) -> Event a
updateInput (Base a) (be, b) = if a === be then 
      case (cast b) of
          Just b1 -> Pure b1
          Nothing -> Base a
   else Base a
updateInput (Sum e1 e2) r = Sum (updateInput e1 r) (updateInput e2 r)
updateInput (Prod f e) r = Prod (updateInput f r) (updateInput e r)
updateInput (Pure e) _ = Pure e
updateInput (Empty) _ = Empty

--updateInput :: (Typeable b) => Event a -> (BEvent b, b) -> IO (Event a)
--updateInput (Base a) (be, b) = do
--  if a === be
--   then do
--      putStrLn "1"
--      case (cast b) of
--          Just b1 -> do
--             putStrLn "2"
--             return $ Pure b1
--          Nothing -> do
--             putStrLn "3"
--             return $ (Base a)
--   else do
--      putStrLn "4"
--      return (Base a)
--updateInput (Sum e1 e2) r = do
--   a <- (updateInput e1 r) 
--   b <- (updateInput e2 r)
--   return $ Sum a b
--updateInput (Prod f e) r = do
--   f' <- updateInput f r
--   e' <-  (updateInput e r)
--   return $ Prod f' e'
--updateInput (Pure e) _ = return $ Pure e
--updateInput (Empty) _ = return Empty

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

--getBases :: Event a -> [BEvents]
--getBases (Base a) = [BEvents a]


--updateInput (Sum a b) = do
--   na <- (updateInput a)
--   nb <- (updateInput b)
--   return $ na <|> nb
--updateInput (Prod f b) = do
--   nb <- (updateInput b)
--   return $ f <*> b
--updateInput (Bind a f) = do
--   na <- (updateInput a) 
--   return $ f na   

data BEvents where
    BEvents :: (Typeable e) => {evs :: BEvent e} -> BEvents

deriving instance Show BEvents

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

--data EitherB a b = LeftB a | RightB b
--instance ErrorList BEvents where
----   listMsg = const []
--
getEventValue :: Event a -> BEither [BEvents] a
getEventValue (Pure a) = BE (Right a)
getEventValue Empty = BE (Left [])
getEventValue (Sum a b) = (getEventValue a) <|> (getEventValue b)
getEventValue (Prod f b) = (getEventValue f) <*> (getEventValue b)
getEventValue (Base a) = BE (Left [BEvents a])


--updateInputs :: StateT EventHandler IO ()
--updateInputs = do 
--   (EventHandler e h) <- get
--   e' <- liftIO $ updateInput e
--   put $ EventHandler e' h
--
--triggerEvents :: EventHandler -> StateT Game IO ()
--triggerEvents (EventHandler e h) = do
--   let mval = getEventValue e
--   when (isJust mval) $ do
--      evalNomex $ h $ fromJust mval
--      events .= []
--
--runGame :: StateT Game IO ()
--runGame = do
--   g <- get
--   liftIO $ putStrLn $ show $ _outputs g
--   zoom (events . traverse) updateInputs
--   mapM_ triggerEvents (_events g)
--
--play :: Nomex ()
--play = OnEvent onInputMyRecord (Output . show)
--
main = undefined
--   let g = Game [] []
--   now <- getCurrentTime
--   execStateT (evalNomex (callVote $ addUTCTime 5 now) >> forever runGame) g
--
--updateInput' :: (Typeable a, Typeable b, Eq a, Eq b) => Event a -> Event b -> Event b -> Event a
--updateInput' e1 e2 b = case (cast e1) of
--   Just e2 -> if (e1 == e2) then e2 else e1
--      --case (cast (Pure b)) of
--      --Just b1 -> Just b1
--      --Nothing -> e1
--   Nothing -> e1
--updateInput' (OnInputText pn1) (OnInputText pn2) b = if (pn1 == pn2) then Pure b else (OnInputText pn1)
--updateInput' (OnInputCheckbox pn1) (OnInputCheckbox pn2) b = if (pn1 == pn2) then Pure b else (OnInputCheckbox pn1)
--updateInput' (OnInputButton pn1) (OnInputButton pn2) b = if (pn1 == pn2) then Pure b else (OnInputButton pn1)
--updateInput' (OnTime t1) (OnTime t2) b = if (t1 == t2) then Pure b else (OnTime t1)
--updateInput' (Sum e1 e1) (Sum e3 e4) b = if (e1 == e3) && (e2 == e4) then Pure b else (Sum (updateInput' e1) (updateInput' e2))
--updateInput' (Prod f1 e1) (Prod f2 e2) b = if (e1 == e2) then Pure b else (Prod f (updateInput' e2)
--updateInput' (Pure a1) (Pure a2) b = if (cast a1 == Just a2) then Pure b else (Pure a1)
--updateInput' (Empty) (Empty) b = Empty
