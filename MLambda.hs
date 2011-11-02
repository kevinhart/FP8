-- | Lambda Calculus, implemented using a 'State' monad.
module MLambda (
-- * State monad
  State
, apply
, getState
, putState
-- * Lambda Calculus functions
, alpha
, unalpha
, beta
) where

import Lambda hiding (alpha, beta)

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

-- | @do uexpr <- alpha expr@ attaches unique numbers to all names in an
-- s-expression; positive exactly if the name is bound to a parameter.
--
-- >>> apply (alpha (read "(lambda ('x') ('x' 'y'))" :: SExpr Char)) ([],0)
-- (lambda ((1,'x')) ((1,'x') (0,'y')))
alpha :: (Num n, Eq a) => SExpr a -> State ([((n,a), (n,a))], n) (SExpr (n, a))
alpha = alpha' . wrap

alpha' :: (Num n, Eq a) => SExpr (n, a) ->
                           State ([((n,a), (n,a))], n) (SExpr (n,a))
alpha' (Proc parm@(_,name) body) = do
  (stack, uniq) <- getState
  let parm' = (uniq+1, name)
  putState (((parm, parm'):stack), uniq+1)
  body' <- alpha' body
  (_, uniq') <- getState    -- pop (parm, parm') off the stack before going
  putState (stack, uniq')   -- up a level, but save uniq'
  return (Proc parm' body')

alpha' var@(Name name) = do
  (stack, uniq) <- getState
  case filter ((name ==).fst) stack of
    (_, parm):_ -> return (Name parm) -- bound variable
    _           -> return var         -- free variable

alpha' (Call proc arg) = do
  (stack, uniq) <- getState
  proc' <- alpha' proc
  arg'  <- alpha' arg
  return (Call proc' arg')

-- | @unalpha uexpr@ reverses the effect of alpha, i.e., removes the unique
-- numbers which 'alpha' attaches to all names in an s-expression
-- >>> unalpha (alpha (read "(lambda ('x') ('x' 'y'))" :: SExpr Char))
-- (lambda ('x') ('x' 'y'))
unalpha :: Num n => State ([a], n) (SExpr (b, c)) -> SExpr c
unalpha = unwrap . ((flip$apply) ([],0))

-- | @beta expr@ reduces an s-expression by applying a procedure to an argument
-- until nothing changes.  Uses structural induction on the s-expression.
-- Performs an 'alpha' conversion first and whenever an argument is substituted.
-- >>> beta (read "(((lambda ('x') (lambda ('y') 'x')) 'y') 'f')" :: SExpr Char)
-- 'y'
beta :: Eq a => SExpr a -> SExpr a
beta expr = unalpha $ do
  subbed <- alpha expr
  loop subbed where
  
    -- reduce until nothing changes anymore
    loop expr = do
      expr' <- beta' expr
      if expr' == expr then return expr else loop expr'

    beta' var@(Name _)    = return var
    beta' proc@(Proc _ _) = return proc
    beta' (Call proc arg) = do
      expr <- beta' proc
      case expr of
        Proc parm body -> subst parm arg body
	exprr          -> return (Call exprr arg)

    -- structural induction to substitute arg for parm
    subst parm arg = subst' where
      subst' var@(Name name)
        | name == parm  = do
	    (_, uniq) <- getState
	    putState ([], uniq)  -- perform sub-alpha with empty stack
	    alpha' arg
	| otherwise     = return var
      subst' (Proc parm body) = do
        body' <- subst' body
	return (Proc parm body')
      subst' (Call proc arg)  = do
        proc' <- subst' proc
	arg'  <- subst' arg
	return (Call proc' arg')
