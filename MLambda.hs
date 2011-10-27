-- | Lambda Calculus, implemented using a 'State' monad.
module MLambda (
-- | State monad
  State
, apply
, getState
, putState
) where

-- | @State@ is a state monad which wraps a computation affecting a state
data State s a = State (s -> (a, s))

-- | @apply computation initial@ applies the wrapped computation to an initial
-- state and returns the result of the computation.
apply :: State s a -> s -> a
apply (State m) init = fst $ m init

-- | @do state <- getState@ produces the current state value.
getState :: State s s
getState = State (\c -> (c, c))

-- | @do putState state@ sets a new state value
putState :: s -> State s ()
putState state = State (\_ -> ((), state))

instance Monad (State s) where
  return x = State (\c -> (x,c))
  State m >>= f = State (\c -> case m c of
    (a, astate) -> case f a of
      State n -> n astate)
