{-# LANGUAGE TupleSections #-}

module Command.Internal.State (
    State(State, runState),
    get, put
) where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State fs) = State $ \s ->
        let (a, s') = fs s
        in (f a, s')

instance Applicative (State s) where
    pure a = State (a,)
    (State f) <*> (State x) = State $ \s ->
        let (ff, s1) = f s
            (xx, s2) = x s1
        in (ff xx, s2)

instance Monad (State s) where
    return a = State (a,)
    (State st) >>= f = State $ \s ->
        let (a, s') = st s
            newSt = f a
        in runState newSt s'

put :: s -> State s ()
put s = State $ const ((), s)

get :: State s s
get = State $ \s -> (s, s)

