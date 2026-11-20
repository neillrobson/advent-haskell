{-# LANGUAGE InstanceSigs #-}

module Sandbox.State () where

import Control.Applicative (Alternative (..))

-- Re-implementing State, along with type classes,
-- to build intuition for the Alternative type class.

newtype State s a = MkState {runState :: s -> (a, s)}

state :: (s -> (a, s)) -> State s a
state = MkState

evalState :: State s a -> s -> a
evalState (MkState f) = fst . f

execState :: State s a -> s -> s
execState (MkState f) = snd . f

--------------------------------------------------------------------------------

-- Using State

parseX :: State String Bool
parseX = MkState $ \case
  ('x' : s) -> (True, s)
  s -> (False, s)

-- runState parseX "xavier" == (True, "avier")
-- runState parseX "avier" == (False, "avier")

parseO :: State String Bool
parseO = MkState $ \case
  ('o' : s) -> (True, s)
  s -> (False, s)

--------------------------------------------------------------------------------

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (MkState st) = MkState $ \s -> let (a, s') = st s in (f a, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = MkState (a,)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (MkState sab) <*> (MkState sa) = MkState $ \s ->
    let (ab, s') = sab s
        (a, s'') = sa s'
     in (ab a, s'')

-- Open question: what does it mean for a State to have a function as a result
-- value?

--------------------------------------------------------------------------------

-- Alternative doesn't make sense for a "pure" State type, because there isn't a
-- failure mode across all output values!

instance Alternative (State s) where
  empty :: State s a
  empty = undefined
  (<|>) :: State s a -> State s a -> State s a
  (<|>) = undefined
