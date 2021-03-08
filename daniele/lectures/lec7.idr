module Lecture7
-- Totality for Data and Codata

-- In idris2 you need to import Data.Stream

namespace nat -- You can declare "private modules"
  -- Recall the inductive type of natural numbers:
  data Nat' : Type where
    Z : Nat'
    S : Nat' -> Nat'
-- Idris functions, including constructors,
-- evaluate their arguments eagerly

-- So every "Nat" is a finite term, S $ S $ ... $ S Z
three : Nat
three = S $ 1 + 1

-- What if we try to write a term that is not finite?
badNat : Nat
badNat = S badNat
-- Idris is not complaining, but this doesn't terminate!

-- A recursive function on an inductive data type is called *Total*
-- if it's:
-- 1) Covering
-- 2) Terminating

-- * Coverage is easy to decide mechanically:
--   just check for all constructors
-- * Termination is impossible to decide mechanically
--   (cf. the halting problem)

-- Idris uses a simple syntactic "conservative approximation"
-- to termination for recursive functions on inductive types:
-- each recursive call must be on a proper subterm 

-- The Fibonacci function:
fib' : Nat -> Nat
fib' Z = 0
fib' (S Z) = 1
fib' (S (S k)) = fib' k + fib' (S k)
-- (this is slow and inefficient)

-- Iterate a function n times:
public export
iterate' : Nat -> (a -> a) -> a -> a
iterate' Z f = id
iterate' (S k) f = iterate' k f . f
-- λΠ> :total iterate'
-- Lecture7.iterate' is Total

-- An infinite recursion:
public export
forever : a -> a
forever x = forever x
-- λΠ> :total forever
-- Lecture7.forever is possibly not total due to recursive path:
--     Lecture7.forever, Lecture7.forever

-- For many programming tasks (e.g. servers)
-- we don't want termination.

-- A process should keep running indefinitely
-- until we shut it down

-- We call such types "coinductive data types"
-- or "codata types" for short.

-- A term of a codata type may be finite or infinite.

-- In Idris we indicate that a term
-- is being used coinductively with the 
-- type constructor** "Inf : Type -> Type"

public export
data CoNat : Type where
  Zero : CoNat
  Succ : (n : Inf CoNat) -> CoNat

-- We get a trm of type "Inf CoNat" by using the
-- term constructor** "Delay : a -> Inf a"
--     **This is not exactly true

one : CoNat
one = Succ $ Delay Zero

-- Idris can automatically insert "Delay"'s for you:

two : CoNat
two = Succ one
-- λΠ> :printdef two
-- two : CoNat
-- two = Succ (Delay one)
-- In idris2 this is a bit different

-- total because recursion is on a subterm:
public export
coN : Nat -> CoNat
coN Z = Zero
coN (S k) = Succ (coN k)

-- Because a term of type "Inf a" is potentially infinite,
-- idris does not evaluate inside a "Delay" until needed

-- λΠ> coN 19
-- Succ (Delay (coN 18)) : CoNat

-- Evaluation within a "Delay" is forced by pattern matching,
-- Which reveals the constructyor form of a term (hnf in lambda calculus)

-- So idris evaluates a term of type "CoNat" only until it
-- discovers whether is "Zero" or a "Succ (Delay n)"

public export
infinity : CoNat
infinity = Succ infinity
-- λΠ> infinity
-- Succ (Delay infinity) : CoNat

-- Pattern matching on a "CoNat" forces the outermost "Delay"
pred : CoNat -> CoNat
pred Zero = Zero
pred (Succ n) = n -- this is actually pred (Succ (Delay n))

still_infinity : CoNat
still_infinity = iterate' 100 pred infinity
-- λΠ> still_infinity
-- Succ (Delay infinity) : CoNat
-- This evaluates!

-- A recursive function on a coinductive type is called *Total*
-- if it's:
-- 1) Covering
-- 2) Productive

-- Productive means that it will evaluate to a
-- (possibly "Delay"ed) result in finite time.
-- Like termination for inductive types this
-- is generally undecidable

-- Idris uses a simple syntactic conservative
-- approximation to producctivity for recursive
-- functions on codata types:
-- each recursive call must be "guarded" by a constructor,
-- and thus by a(n implicit) "Delay".

-- The recursive call to "plus" is
-- guarded by the constructor "Succ":
public export
plus : CoNat -> CoNat -> CoNat
plus Zero y = y
plus (Succ n) y = Succ (plus n y)
-- λΠ> :total plus
-- Lecture7.plus is Total

even_more_infinite : CoNat
even_more_infinite = plus infinity infinity
-- λΠ> even_more_infinite
-- Succ (Delay (plus infinity (Succ (Delay infinity)))) : CoNat

-- Not total becayse recyrsuib us bit constructor-guarded:
public export
uncoN : CoNat -> Nat
uncoN Zero = Z
uncoN (Succ n) = S (uncoN n)
-- λΠ> :total uncoN
-- Lecture7.uncoN is possibly not total due to recursive path:
--     Lecture7.uncoN, Lecture7.uncoN
-- S is a constructor for "Nat" and doesn't have a "Delay"

-- Possible non-totality is infectious:
badNat' : Nat
badNat' = uncoN infinity
-- This obviously doesn't terminate

-- Warning: in the current implementations of idris (both 1 and 2),
-- productivity recognition is pretty fragile
-- Each subterm of a codata type must be an *immediate*
-- constructor argument

-- Should be productive, but not recognized:
infinity' : CoNat
infinity' = Succ (Delay (id infinity'))
-- Lecture7.infinity' is possibly not total due to recursive path:
--    Lecture7.infinity'


-- Recognized as possibly non-terminating by in bla bla lost it
---------------------------------------------------------------
---------------------------------------------------------------
---------------------------------------------------------------

-- Evaluating total fucntions on a total argument should be safe
-- but idris currently does not try to do this

-- Terminating but not recognized:
stops : a -> a
stops = iterate' 0 forever




namespace list
-- Recall the type of finite sequences:
  data List' : Type -> Type where
    Nil : List' a
    (::) : a -> List' a -> List' a
    
-- The type of infinite or finite sequences:
public export
data CoList : Type -> Type where
  Nil : CoList a
  (::) : (x : a) -> (xs : Inf (CoList a)) -> CoList a
  
-- Recognized as productive for "CoList Nat"
public export
zeros : CoList Nat
zeros = 0 :: zeros -- (Delay zeros) is implicit
-- λΠ> :total zeros
-- Lecture7.zeros is Total

-- Recognized as terminating for "Nat"
public export
take : Nat -> CoList a -> List a
take Z x = []
take (S k) [] = []
take (S k) (x :: xs) = x :: take k xs
-- λΠ> :total take
-- Lecture7.take is Total

-- Recognized as productive for "CoList b":
public export
mapCoList : (a -> b) -> CoList a -> CoList b
mapCoList f [] = []
mapCoList f (x :: xs) = f x :: (mapCoList f xs)
-- λΠ> :total mapCoList
-- Lecture7.mapCoList is Total

-- A concise idiomatic definition:
-- Should be productive but not recognized:
nats' : CoList Nat
nats' = 0 :: mapCoList S nats'
-- λΠ> take 10 nats'
-- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] : List Nat
-- But:
-- λΠ> :total nats'
-- Lecture7.nats' is possibly not total due to recursive path:
--     Lecture7.nats'

-- Often, we can rewrite such definitions
-- so that idris can recognize totality:
public export
nats : CoList Nat
nats = nats_helper 0 where 
       nats_helper: Nat -> CoList Nat
       nats_helper k = k :: nats_helper (S k)
-- λΠ> :total nats
-- Lecture7.nats is Total

-- The hailstone function:
-- (n/2) ecc ecc

-- Unisng "Integer"s because "Nat" arithmetic is slow
h : Integer -> Integer
h x = case (mod x 2) == 0 of
  True => div x 2
  False => (3 * x) + 1

-- The hailstone sequence:
-- hail (n) = [n, h n, h (h n), ...]

-- We can stop if we ever reach 0 or 1
-- Because of the cycles (0) and (1, 4, 2)
hail : Integer -> CoList Integer
hail n = case n < 2 of
  True => [n]
  False => n :: hail (h n)

hail_seventeen : List Integer
hail_seventeen = take 13 $ hail 17

-- Sometimes we do know that a sequence of data
-- will be infinite
-- In this case, we don't want a "Nil" constructor

-- This is in the standard library as Prelude.Stream
namespace stream
  data Stream' : Type -> Type where
    (::) : a -> Inf (Stream' a) -> Stream' a
    
public export
natStream : Stream Nat
natStream = nats_helper 0 where 
       nats_helper: Nat -> Stream Nat
       nats_helper k = k :: nats_helper (S k)

-- We can efficiently compute "fib n"
-- if we know both "fib n-1" and "fib n-2"

-- So we construct a stream of (fib n, fib (S n)) pairs:
--
--  n :       0   1   2   3   4   5  ...
--  --       --  --  --  --  --  --
--  fib n:   0    1   1   2   3   5  ...
--  fib S n: 1    1   2   3   5   8  ...

fibStream : Stream (Pair Nat Nat)
fibStream = (0, 1) :: map next fibStream
            where
              next : Pair Nat Nat -> Pair Nat Nat
              next (fib_pp, fib_p) = (fib_p, fib_pp + fib_p)

-- Not actually very fast due to the "Nat" arithmetic
-- but relative to "fib" above:
fast_fib : Nat -> Nat
fast_fib n = (Prelude.Basics.fst . head . drop n) fibStream
-- In idris 1 this is not actually true?
