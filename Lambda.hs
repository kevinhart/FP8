-- | Lambda Calculus with Scheme-like notation.

module Lambda (
     -- * Types
     Symbol, SExpr(Name, Proc, Call), Macro, Macros,
     
     -- * Functions
     substitute, wrap, unwrap, alpha, beta,
     
     -- * Examples from assignment 2
     e1, e2, e3, e4, e5, e6, e7, e8,
     m1, m2, m3, m4, m5, m6, m7, m8,
     run
  ) where
          
import Char

-----------------------------------------------------------------------
-- | @ Symbol \"symbol\" @
--   represents an alphanumeric symbol as a string
--   which can be read and shown as alphanumerics without quotes.
--
-- >>> read "a" :: Symbol
-- a

newtype Symbol = Symbol String deriving Eq

-- show Symbol
-- without the quotes.

instance Show Symbol where
  showsPrec _ (Symbol s) = (s ++)

-- read :: Symbol
-- expects to parse alphanumeric symbol only.

instance Read Symbol where
  readsPrec _ s = case lex s of
    [(a,b)] -> if all isAlphaNum a then [(Symbol a, b)] else []
    _       -> []

-----------------------------------------------------------------------
-- | @ SExpr @
--   represents s-expressions (from assignment 2).
--
-- >>> read "a" :: SExpr Symbol
-- a
-- >>> read "(lambda (x) x)" :: SExpr Symbol
-- (lambda (x) x)
-- >>> read "(foo bar)" :: SExpr Symbol
-- (foo bar)

data SExpr a
  = Name a                    -- ^ represents a variable.
  | Proc a (SExpr a)          -- ^ represents a procedure.
  | Call (SExpr a) (SExpr a)  -- ^ represents a procedure call.
  deriving Eq

-- show (SExpr a)
-- in Scheme notation.

instance Show a => Show (SExpr a) where
  showsPrec _ (Name name)      = shows name
  showsPrec _ (Proc name body) =
    ("(lambda ("++) . shows name . (") "++) . shows body . (')':)
  showsPrec _ (Call fun arg)   =
    ('(':) . shows fun . (' ':) . shows arg . (')':)

-- read :: SExpr a
-- from Scheme notation.

instance Read a => Read (SExpr a) where
  readsPrec n s =
  --                           pattern rest       lex input
    [(Proc name body, tail) | ("(",      t1)   <- lex s,
                              ("lambda", t2)   <- lex t1,
                              ("(",      t3)   <- lex t2,
                              (name,     t4)   <- reads t3,
                              (")",      t5)   <- lex t4,
                              (body,     t6)   <- readsPrec n t5,
                              (")",      tail) <- lex t6]
    ++
    [(Call fun arg, tail)   | ("(",      t1)   <- lex s,
                              (fun,      t2)   <- readsPrec n t1,
                              (arg,      t3)   <- readsPrec n t2,
                              (")",      tail) <- lex t3]
    ++
    [(Name name, tail)      | (name,     tail) <- reads s]

-----------------------------------------------------------------------
-- | @ Macro @
--   represents one macro, i.e., it maps a name to an s-expression.
--
-- >>> read "(define a b)" :: SExpr Symbol
-- (define a b)

data Macro a = Macro a (SExpr a)

-- show (Macro a)
-- in Scheme notation.

instance Show a => Show (Macro a) where
  showsPrec _ (Macro name expr) =
    ("(define "++) . shows name . (" "++) . shows expr . (')':)

-- read :: Macro a
-- from Scheme notation.

instance Read a => Read (Macro a) where
  readsPrec n s =
  --                            pattern rest       lex input
    [(Macro name expr, tail) | ("(",      t1)   <- lex s,
                               ("define", t2)   <- lex t1,
                               (name,     t3)   <- reads t2,
                               (expr,     t4)   <- reads t3,
                               (")",      tail) <- lex t4]

-----------------------------------------------------------------------
-- | @ Macros @
-- is a list of macros, i.e., names mapping to s-expressions.
-- Names should be different from all parameters; no self-reference.

type Macros a = [Macro a]

-----------------------------------------------------------------------
-- | @ substitute macros expr @
--   returns an s-expression with names replaced by macro values.
--   Implemented by structural induction on the s-expression;
--   would loop on a self-referential substitution.
--
-- >>> substitute (map read ["(define a b)"]) (read "a" :: SExpr Symbol)
-- b

substitute :: (Eq a, Show a) => Macros a -> SExpr a -> SExpr a

substitute macros expr = subst expr where

  -- replace name by first definition for name, if any, and recurse
  subst var@(Name name)  =
    case filter (\(Macro m expr) -> m == name) macros of
      (Macro _ expr):_ -> subst expr
      []               -> var

  -- recurse into procedure body or call
  subst (Proc parm body) = Proc parm (subst body)
  subst (Call proc arg)  = Call (subst proc) (subst arg)

-----------------------------------------------------------------------
-- | @ alpha expr @
--   attaches unique numbers to all names;
--   positive exactly if the name is bound to a parameter.
--
-- >>> alpha (read "(lambda (x) (x y))" :: SExpr Symbol)
-- (lambda ((1,x)) ((1,x) (0,y)))
-- >>> unwrap it
-- (lambda (x) (x y))

alpha :: (Num n, Eq a) => SExpr a -> SExpr (n, a)

alpha = snd . alpha' [] 0 . wrap

-----------------------------------------------------------------------
-- | @ wrap expr @
--   attaches zero to all names.
--   Converts @ ('SExpr' name) @ to @ ('SExpr' (count, name)) @
--   using structural induction on the s-expression.
--
-- >>> wrap (read "(lambda (x) (x y))" :: SExpr Symbol)
-- (lambda ((0,x)) ((0,x) (0,y)))

wrap :: (Num n, Eq a) => SExpr a -> SExpr (n, a)

wrap (Name name)      = Name (0, name)
wrap (Proc parm body) = Proc (0, parm) (wrap body)
wrap (Call proc arg)  = Call (wrap proc) (wrap arg)

-- | @ unwrap uexpr @
--   reverses the effect of 'wrap' (and 'alpha').
--   Uses structural induction on the s-expression.

unwrap :: SExpr (n, a) -> SExpr a

unwrap (Name (_, name))      = Name name
unwrap (Proc (_, parm) body) = Proc parm (unwrap body)
unwrap (Call proc arg)       = Call (unwrap proc) (unwrap arg)

-----------------------------------------------------------------------
-- | @ alpha' stack counter (SExpr (count, name)) @
--   takes a stack of parameter names, initially empty,
--   a counter to make names unique, and an s-expression;
--   returns the new value of the counter and
--   the s-expression with new, unique counter values
--   attached to each name which is bound by a parameter.
--   Implemented as an environment-passing interpreter
--   using structural induction on the s-expression
--   while passing the unique count.

alpha' :: (Num n, Eq a) => [((n, a), (n, a))] -> n
            -> SExpr (n, a) -> (n, SExpr (n, a))

alpha' stack uniq (Proc parm@(_, name) body) =
  (uniq', Proc parm' body') where
    parm'          = (uniq+1, name)
    (uniq', body') = alpha' ((parm, parm') : stack) (uniq+1) body

alpha' stack uniq var@(Name name) =
  case filter ((name ==).fst) stack of
    (_, parm):_ -> (uniq, Name parm) -- bound variable
    _           -> (uniq, var)       -- free variable

alpha' stack uniq (Call proc arg) =
  (uniq'', Call proc' arg') where
    (uniq', proc') = alpha' stack uniq proc
    (uniq'', arg') = alpha' stack uniq' arg

-----------------------------------------------------------------------
-- | @ beta expr @
--   reduces an s-expression by applying a procedure to an argument
--   until nothing changes. Uses structural induction on the
--   s-expression. Performs an 'alpha' conversion first and whenever an
--   argument is substituted.
--
-- >>> beta (read "(((lambda (x) (lambda (y) x)) y) f)" :: SExpr Symbol)
-- y

beta :: (Eq a) => SExpr a -> SExpr a

beta = unwrap . snd . loop . alpha' [] 0 . wrap where

  -- reduce until nothing changes anymore
  loop (uniq, expr) = 
    if expr' == expr then (uniq', expr) else loop (uniq', expr')
      where (uniq', expr') = beta uniq expr

  beta uniq var@(Name _)    = (uniq, var)
  beta uniq proc@(Proc _ _) = (uniq, proc)
  beta uniq (Call proc arg) = case beta uniq proc of
      (uniq', Proc parm body) -> subst uniq' parm arg body
      (uniq', expr)           -> (uniq', Call expr arg)

  -- structural induction to substitute arg for parm
  subst uniq parm arg = subst uniq where
    subst uniq var@(Name name)
      | name == parm    = alpha' [] uniq arg
      | otherwise       = (uniq, var)
    subst uniq (Proc parm body) = (uniq', Proc parm body') where
      (uniq', body') = subst uniq body
    subst uniq (Call proc arg)  = (uniq'', Call proc' arg') where
      (uniq', proc') = subst uniq proc
      (uniq'', arg') = subst uniq' arg

-----------------------------------------------------------------------
-- lexical scoping:

m1 = []
e1 = [ " (((lambda (x) (lambda (x) x)) a) b) ",
       " (((lambda (x) (lambda (y) x)) y) foo) " ]
         
-----------------------------------------------------------------------
-- Church booleans:

m2 = [ " (define true  (lambda (x) (lambda (y) x))) ",
       " (define false (lambda (x) (lambda (y) y))) ",
       " (define if    (lambda (cond) (lambda (then) (lambda (else) ((cond then) else))))) " ]

e2 = [ " (((if true) this) that) " ,
       " (((if false) this) that) " ]

-----------------------------------------------------------------------
-- Church numerals:

m3 = m2 ++ [
       " (define 0 (lambda (f) (lambda (x) x))) ",
       " (define 1 (lambda (f) (lambda (x) (f x)))) ",
       " (define 2 (lambda (f) (lambda (x) (f (f x))))) ",
       " (define 3 (lambda (f) (lambda (x) (f (f (f x)))))) " ]

e3 = [ " ((0 (lambda (f) (f hello))) (lambda (x) x)) ",
       " ((1 (lambda (f) (f hello))) (lambda (x) x)) ",
       " ((2 (lambda (f) (f hello))) (lambda (x) x)) ",
       " ((3 (lambda (f) (f hello))) (lambda (x) x)) " ]

-----------------------------------------------------------------------
-- iteration:

m4 = m3 ++ [
       " (define repeat (lambda (n) (lambda (x) ((n (lambda (g) (g x))) (lambda (y) y))))) " ]

e4 = [ " ((repeat 2) hello) " ]

-----------------------------------------------------------------------
-- counting up and down:

m5 = m4 ++ [
       " (define succ (lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))) ",
       " (define pred (lambda (n) (((n (lambda (p) (lambda (z) ((z (succ (p true))) (p true))))) (lambda (z) ((z 0) 0))) false))) ",
       " (define pred2 (lambda (n) (lambda (f) (lambda (x) (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (u) x)) (lambda (u) u)))))) " ]

e5 = [ " ((repeat (succ 2)) hello) ",
       " ((repeat (pred 2)) hello) ",
       " ((repeat (pred2 2)) hello) " ]

-----------------------------------------------------------------------
-- arithmetic:

m6 = m5 ++ [
       " (define sum (lambda (m) (lambda (n) (lambda (f) (lambda (x) ((m f) ((n f) x))))))) ",
       " (define product (lambda (m) (lambda (n) (lambda (f) (m (n f)))))) " ]

e6 = [ " ((repeat ((sum 2) 3)) hello) ",
       " ((repeat ((product 2) 3)) hello) " ]

-----------------------------------------------------------------------
-- predicate:

m7 = m6 ++ [
       " (define isZero (lambda (n) ((n (true false)) true))) " ]

e7 = [ " (((if (isZero 0)) this) that) ",
       " (((if (isZero 1)) this) that) " ]
  
-----------------------------------------------------------------------
-- recursion:

m8 = m7 ++ [
       " (define Y (lambda (y) ((lambda (x) (y (x x))) (lambda (x) (y (x x)))))) ",
       " (define nfact (lambda (f) (lambda (n) (((if (isZero n)) 1) ((product n) (f (pred n))))))) ",
       " (define factorial (Y nfact)) " ]

e8 = [ " ((repeat (factorial ((product 2) 2))) hello) " ]

-----------------------------------------------------------------------
-- | @ run @
--   executes all examples from assignment 2.
--
-- >>> run
-- [[b,y],
--  [this,that],
--  [(lambda (x) x),hello,(hello hello),((hello hello) hello)],
--  [(hello hello)],
--  [((hello hello) hello),hello,hello],
--  [((((hello hello) hello) hello) hello),
--   (((((hello hello) hello) hello) hello) hello)],
--  [this,that],
--  [(((((((((((((((((((((((hello hello) hello) ...)]
-- ]

run :: [[SExpr Symbol]]
run = map (\(macros, expressions) ->
  map (beta.substitute (map read macros).read) expressions) [
    (m1, e1), (m2, e2), (m3, e3), (m4, e4),
    (m5, e5), (m6, e6), (m7, e7), (m8, e8) ]
