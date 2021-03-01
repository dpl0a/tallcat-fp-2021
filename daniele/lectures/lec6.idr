-- REVIEW

-- Each source file corresponds to a namespace, if no module name
-- is specified it defaults to "Main", in idris2 the module name
-- is enforced by the file name (?)
module Lecture6

import Data.Fin
import Data.Vect

-- Inductive Data Types and their Constructors:
data Shape : Type where
  Circle : (radius : Double) -> Shape  -- You can document variables in constructors
  Rectangle : (width , height : Double) -> Shape
  IsoTriangle : (base , height : Double) -> Shape
  RegularPoly : (sides : Nat) -> (length : Double) -> Shape
  
-- Recursive Functions:
area : Shape -> Double
area (Circle radius) = 2 * pi * radius
area (Rectangle width height) = width * height
area (IsoTriangle base height) = base * height / 2
area (RegularPoly sides length) =  
  let
    theta = 2 * pi / cast sides
    -- tan theta = (1/2) length / height
    height = (1 / 2) * length / tan theta
  in
  cast sides * area (IsoTriangle length height)

-- Lost part of the lecture :(

-- This is the same as (Fin n) in Data.Fin
data BoundedNat : (index : Nat) -> Type where
  Zero : BoundedNat (S n)
  Succ : BoundedNat n -> BoundedNat (S n)
  

-- Some type constructors take both parameters and indices:
-- (this is in the standard library as Data.Vect)
data SizedList : (size : Nat) -> (kind : Type) -> Type where
  Nil : SizedList 0 a
  Cons : a -> SizedList n a -> SizedList (S n) a
  
concatVect : Vect m a -> Vect n a -> Vect (m + n) a
concatVect [] ys = ys
concatVect (x :: xs) ys = x :: concatVect xs ys

-- Higher-Order Functions:
-- A higher order function is a function that trafficks in
-- other functions.
-- It might take a function as an argument or return a
-- function as a result.
-- Higher-Order functions let us turn "design patterns"
-- into actual programs.

-- Maybe mapping:
mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just $ f x

-- List mapping:
mapList : (a -> b) -> List a -> List b 
mapList f [] = []
mapList f (x :: xs) = f x :: mapList f xs

-- Maybe zipping
zipMaybe : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
zipMaybe f Nothing y = Nothing
zipMaybe f (Just x) Nothing = Nothing
zipMaybe f (Just x) (Just y) = Just $ f x y

-- List zipping
zipList : (a -> b -> c) -> List a -> List b -> List c
zipList f [] ys = []
zipList f (x :: xs) [] = []
zipList f (x :: xs) (y :: ys) = f x y :: zipList f xs ys


-- Folds are a very general way to write functions
-- from an inductively defined type to any other type.

-- To write the type of the fold for an inductive type:
-- (1) Examine the types of its constructors:
--     [] : List a
--     (::) : a -> List a -> List a
-- (2) In each constructor replace the type itself
--     with a parameter type t:
--     n : t
--     c : a -> t -> t
-- (3) The fold is a function that takes one argument
-- for each such term replacing a constructor
-- and returns a function from the type being folded
-- to the parameter type:
foldList : (c : a -> t -> t) -> (n : t) -> List a -> t
foldList c n [] = n
foldList c n (x :: xs) = c x (foldList c n xs)

-- Many functions can be written as folds:
mapList' : (a -> b) -> List a -> List b
mapList' f = foldList (\x , ys => f x :: ys) []


-- The IO type constructor is used to differentiate
-- * expressions, which get evaluated to a value from
-- * computations, which may perform actions
-- (i.e. interact with the world)

-- The run-time system defines some primitive computations
-- for performing IO actions.
greet : IO Unit
greet = putStrLn "hi!"

-- We build up compound IO computations using:
-- * a trivial computation (pure),
--   which performs no actions and returns a value,
-- * a computation sequencing operator (>>=),
--   which performs the actions of its first computation
--   and passes the resulting value to its next computation
meet_and_greet : IO Unit
meet_and_greet = 
  putStr "What's your name? "
  >>= const getLine
  >>= \name => putStrLn ("Hello " ++ name ++ "!")

-- There is syntactic sugar to write dis
-- in an imperative-looking style:
meet_and_greet' : IO Unit
meet_and_greet' = do
                  putStr "What's your name? "
                  name <- getLine
                  putStrLn ("Hello " ++ name ++ "!")
