import Data.Vect

-- Today we're going to see functions defined on "generic types"
id_ : a -> a
id_ x = x
-- The identity function is the most standard "generic function", it just returns its input

-- Now let's take a look at types that are made of different types, these are called
-- Type Constructors. Here's the sum type:
data Sum : Type -> Type -> Type where
  Left : a -> Sum a b
  Right : b -> Sum a b

-- Let's construct inhabitants of this type
thing_1 : Sum Nat Bool
thing_1 = Left 3

thing_2 : Sum Nat Bool
thing_2 = Right True

-- Great! But Sum is built into idris2 as Either

-- Sum Unit Unit (Sum One One)
thing_l : Sum Unit Unit
thing_l = Left ()

thing_r : Sum Unit Unit
thing_r = Right ()

-- Note that Sum One One ~~ Bool ~~ Two

-- Sum types are a lot like unions in C.

-- The dual of the Sum type is the Product type:
data Prod : Type -> Type -> Type where
  Pair : a -> b -> Prod a b
  
-- Let's build a product:
thing_3 : Prod Nat Nat
thing_3 = Pair 3 5
-- Note that i have to give idris both of the elements, unlike Sum which only needs one

-- We also can use two different types
thing_4 : Prod Bool Nat
thing_4 = Pair True 5

-- In idris this is built in as (,)

-- If i have Product there are two functions: fst and snd, which return the first and second
-- element of a pair respectively. Let's build them ourselves
fst' : (a,b) -> a
fst' (x, y) = x

snd' : (a,b) -> b
snd' (x, y) = y
-- Note that we can also use pattern matching

-- Now let's look at a really important type, the List type:
{-
data Lst : Type -> Type where
  Nil : Lst a
  Cons : a -> Lst a -> Lst a

This is called List in idris

[1,2,3] ~~>
[] ~~> Nil
[3] ~~> (Cons 3 Nil)
[2,3] ~~> (Cons 2 (Cons 3 Nil))
[1,2,3] ~~> (Cons 1 (Cons 2 (Cons 3 Nil)))

We have a special syntax for Cons:
[1,2,3] ~~> (1 :: (2 :: (3 :: Nil)))

Note that elements of List a have to be all of type a
-}

-- Let's define a function on a particular kind of List:
sumList : List Nat -> Nat
sumList [] = 0
sumList (x :: xs) = x + (sumList xs)

-- Now a function on generic lists:
listLen : List a -> Nat
listLen [] = 0
listLen (x :: xs) = 1 + (listLen xs)

-- Question: Is there any difference between the types "List Unit" and "Nat"?

{-
They look the same to me
  Z ~~ Nil
  S a ~~ () :: a
-}

-- Now here's a really nice type 

{-
data Maybe : Type -> Type where
  Nothing : Maybe a
  Just : a -> Maybe a
-}

-- Another way to define monus is by using Maybe:
minus' : Nat -> Nat -> Maybe Nat
minus' k Z = Just k
minus' Z (S k) = Nothing
minus' (S k) (S j) = minus' k j

-- And let's see how we can use Maybe and List:
headList : List a -> Maybe a
headList [] = Nothing
headList (x :: xs) = Just x

tailList : List a -> Maybe (List a)
tailList [] = Nothing
tailList (x :: xs) = Just xs

-- Pick the kth index of a list (but without overflows)
index : List a -> Nat -> Maybe a
index [] k = Nothing
index (x :: xs) Z = Just x
index (x :: xs) (S k) = index xs k

-- Let's say i have two (Maybe Nat)s, what if i want to take the sum of them and return another Maybe Nat? 
maybePlus : Maybe Nat -> Maybe Nat -> Maybe Nat
maybePlus Nothing y = Nothing
maybePlus (Just x) Nothing = Nothing
maybePlus (Just x) (Just y) = Just (x + y)

-- Let's look at lists with fixed size, called Vectors:
{-
data Vec : (n : Nat) -> Type -> Type where
  Nil : Vec 0 a
  Cons : a -> Vec n a -> Vec (S n) a

We're going to use the built in one, called Vect (we've imported it at the top)
-}

-- Now let's write head and tail functions for Vect:
headVect : Vect (S n) a -> a
headVect (x :: xs) = x

tailVect : Vect (S n) a -> Vect n a
tailVect (x :: xs) = xs


{- This type is kinda related to vectors but kinda confusing (this is built in the std library):

(Fin n) is the type of natural numbers less than n

data Fin : Nat -> Type where
  FZ : Fin (S k)             -- 0 < k for any k
  FS : Fin k -> Fin (S k)    -- if x < k, then (S x) < (S k)
  
Huh? Examples please?

Fin n ~~> things < n
FZ : Fin 4 ~~> 0 : < 4
(FS FZ) : Fin 4 ~~ 1 : < 4
(FS (FS FZ)) : Fin 4 ~~> 2 : < 4
(FS (FS FZ)) : Fin 7 ~~> 2 : < 7
-}

{- Fin 0 has no elements
leq_zero : Fin 0
leq_zero = ?leq_zero_rhs

The compiler complains because there's no elements to define this function on
-}

-- Let's apply this to vectors:
indexVect : Vect n a -> Fin n -> a
indexVect [] FZ impossible
indexVect [] (FS x) impossible
indexVect (y :: xs) FZ = y
indexVect (y :: xs) (FS x) = indexVect xs x
-- Huh! That's nice!

-- Now let's append lists:
-- [1,2,3] [4,5,6] ~~> [1,2,3,4,5,6] (append)
appendList : List a -> List a -> List a
appendList [] ys = ys
appendList (x :: xs) ys = x :: (appendList xs ys)


-- And for vectors? Let's see:
appendVect : Vect n a -> Vect m a -> Vect (n + m) a -- Wow! We're applying a function inside a type signature
appendVect [] ys = ys
appendVect (x :: xs) ys = x :: appendVect xs ys


{-
Why is Fin =/= Nat?
Let's start from the observation that Nat : Type while Fin : Nat -> Type
And also
While Fin 100 : Type, Nat has an element for each natural number, i.e. Z, S Z, S (S Z), ...
And Fin 100? Well...
0 : <100
1 : <100
...
99 : <100
___no more___
-} 

-- Note that Bool ~~ Fin 2
