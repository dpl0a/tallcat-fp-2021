-- Lambda expressions
-- We can define functions by using lambda-notation, even without giving them a name

-- (\x => x)

-- Until now we wrote functions of type
-- foo : a -> b -> a -> b
-- foo x y z = y

-- We can also define higher order functions, i.e. functions that
-- take functions as their arguments/return functions
-- apply : (a -> b) -> a -> b
-- apply f x = f x -- id of type (a -> b)
-- It's obvious that "apply f = f", lmao

-- The "->" operator is _right associative

-- add' : Nat -> (Nat -> Nat) -- Same as "Nat -> Nat -> Nat"
-- add' k j = k + j

-- Look for ":t (+) 3" in the REPL
-- λΠ> :t (+) 3
-- (+) 3 : Integer -> Integer
-- This is a function of type Integer -> Integer

-- Function application is _left-associative_, i.e.
-- These are the same
-- (f x1 x2 x3) ~~ (((f x1) x2) x3)

-- Let's rewrite apply
apply : (a -> b) -> a -> b
apply = id -- id of type (a -> b), now in pointfree notation

-- "functions are values if we have higher-order functions"

-- Let's rewrite add' with lambda-notation
add' : Nat -> (Nat -> Nat) -- Same as "Nat -> Nat -> Nat"
add' = (\x => (\y => x + y)) -- Same as (\x, y => x + y)

-- Let's take "f : a -> b"
-- xs : List a
-- xs = x1, x2, ..., xn
-- We want to apply f to all the elements of xs

mapList : (a -> b) -> List a -> List b
mapList f [] = []
mapList f (x :: xs) = f x :: (mapList f xs)

-- What about mapping inside Maybe?
mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

-- Alternatively
mapMaybe' : (a -> b) -> Maybe a -> Maybe b
mapMaybe' f x = case x of -- Another kind of pattern matching
                  Nothing => Nothing
                  (Just y) => Just (f y)
                  
-- This is just syntactic sugar for 
-- mapMaybe' : (a -> b) -> Maybe a -> Maybe b
-- mapMaybe' f x = case (id x) of -- We can use other functions instead of "id"
--                   Nothing => Nothing
--                   (Just y) => Just (f y)

-- Let's look at branch
--branch : Bool -> a -> a -> a
-- branch x y z = if x then y else z

-- branch' : Bool -> a -> a -> a
-- branch' x y z = 

-- Another useful HOF:
--         predicate
filter' : (a -> Bool) -> List a -> List a
filter' p [] = []
filter' p (x :: xs) = case p x of
                       True => x :: (filter p xs)
                       False => (filter p xs) 
-- We can also split on more than two cases

-- The "fold" for a type replaces its constructors with functions, e.g.
--          Nothing    Just
foldMaybe : b       -> (a -> b) -> Maybe a -> b
foldMaybe no ju Nothing = no
foldMaybe no ju (Just x) = ju x

-- i.e. λΠ> foldMaybe Z S Nothing
--      0 : Nat
--      λΠ> foldMaybe Z S (Just 5)
--      6 : Nat

-- Let's see a more useful instance of "fold"
--         Nil    Cons
foldList : b   -> (a -> b -> b) -> List a -> b
foldList ni co [] = ni
foldList ni co (x :: xs) = co x (foldList ni co xs) -- this is foldr

-- Kinda confusing, let's see an example:
-- λΠ> foldList Z (+) [1,2,3,4,5]
-- 15 : Nat

-- Let's see how a list is made and how "foldList Z (+)" acts on it:
{-
   ::                          +
  /  \                        / \
a1   ::                      a1  +
    /  \                        / \
  a2    .        ~~>           a2  .
         .                          .
          .                          . 
          ::                         +
         /  \                       / \
        an  []                     an  0
-}

-- Another application of foldList: flattening lists of lists
flattenList : List (List a) -> List a
flattenList = foldList [] (++)

-- We can write map in terms of fold:
mapList' : (a -> b) -> List a -> List b
mapList' f = foldList [] (\x, xs => f x :: xs)

-- Another instance of "fold"
--        Z    S
foldNat : b -> (b -> b) -> Nat -> b
foldNat zf sf Z = zf
foldNat zf sf (S k) = sf (foldNat zf sf k)

-- Let's redefine addition by using "foldNat"
add'' : Nat -> Nat -> Nat
add'' a b = foldNat b S a

-- Also multiplication
mul'' : Nat -> Nat -> Nat
mul'' a = foldNat 0 (+a)

-- Another example
toUnit : Nat -> List Unit 
toUnit = foldNat [] (\xs => () :: xs)
