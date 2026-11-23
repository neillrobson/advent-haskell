{-# LANGUAGE InstanceSigs #-}

module Sandbox.State () where

import Control.Applicative (Alternative (..))
import Data.Bifunctor (first)

-- Re-implementing State, along with type classes,
-- to build intuition for the Alternative type class.

newtype State s a = MkState (s -> (a, s))

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

-- Alternative doesn't make sense for a "pure" State type, because there isn't a
-- failure mode across all output values! See the "Parser" type below.

instance Alternative (State s) where
  empty :: State s a
  empty = undefined
  (<|>) :: State s a -> State s a -> State s a
  (<|>) = undefined

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (MkState sa) >>= f = MkState $ \s ->
    let (a, s') = sa s
        (MkState sb) = f a
     in sb s'

--------------------------------------------------------------------------------

newtype Parser s a = MkParser (s -> Maybe (a, s))

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (MkParser sa) = MkParser (fmap (first f) . sa)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = MkParser $ \s -> Just (a, s)
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (MkParser sab) <*> (MkParser sa) = MkParser $ \s -> do
    (ab, s') <- sab s
    (a, s'') <- sa s'
    return (ab a, s'')

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = MkParser $ const Nothing
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (MkParser sa) <|> (MkParser sb) = MkParser $ \s -> sa s <|> sb s

-- some v = (:) <$> v <*> many v
-- many v = some v <|> pure []

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (MkParser sa) >>= f = MkParser $ \s -> do
    (a, s') <- sa s
    let (MkParser sb) = f a
    sb s'
