import Data.Vect
import Data.Fin

-- Task 1:
-- Augment the "Shape" type from lecture 6 with a constructor "Star" for n-pointed
-- stars, where an n-pointed star of length l and height h consists of an n-sided
-- regular polygon of face length l with an isosceles triangle of base l and height
-- h attached along each face
data Shape : Type where
  Circle : (radius : Double) -> Shape  -- You can document variables in constructors
  Rectangle : (width , height : Double) -> Shape
  IsoTriangle : (base , height : Double) -> Shape
  RegularPoly : (sides : Nat) -> (length : Double) -> Shape
  Star : (points : Nat) -> (length, height : Double) -> Shape



-- Task 2:
-- Update the "area" function to be compatible with your new definition of "Shape"
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
area (Star points length height) = (area $ RegularPoly points length) + cast points * (area $ IsoTriangle length height)



-- Task 3:
-- Write a function "indexList : (index : Nat) -> List a -> Maybe a" which returns
-- the element at the specified index of a list, if any
indexList : (index : Nat) -> List a -> Maybe a 
indexList index [] = Nothing
indexList Z (x :: xs) = Just x
indexList (S k) (x :: xs) = indexList k xs



-- Task 4:
-- write a function "indexVect : (index : Fin n) -> Vect n a -> a" which returns 
-- the element at the specified index of a vector. Why do we not need "Maybe" in 
-- the return type?
indexVect : (index : Fin n) -> Vect n a -> a
indexVect FZ (x :: xs) = x
indexVect (FS x) (y :: xs) = indexVect x xs
-- We don't need "Maybe" in the signature of this function because the index
-- is of type "Fin n", so for example:
-- λΠ> indexVect 199 [1,2,3]
-- (input):1:11-13:When checking argument prf to function Data.Fin.fromInteger:
--         When using 199 as a literal for a Fin 3 
--                 199 is not strictly less than 3



-- Task 5:
-- Recall the definition of "Tree a" from lecture 6
data Tree : Type -> Type where
  Leaf : Tree a
  Node : (left : Tree a) -> (this : a) -> (right : Tree a) -> Tree a
-- Write a zip function for trees
zipTree : (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipTree f Leaf y = Leaf
zipTree f (Node left this right) Leaf = Leaf
zipTree f (Node left this right) (Node x y z) = Node (zipTree f left x) (f this y) (zipTree f right z)



-- Task 6:
-- Write the fold function for the parametrized type "Maybe a"
foldMaybe : (no : b) -> (ju : a -> b) -> Maybe a -> b
foldMaybe no ju Nothing = no
foldMaybe no ju (Just x) = ju x



-- Task 7:
-- Use "foldMaybe" to write a map for "Maybe a" as a one liner
mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f = foldMaybe Nothing (\x => Just $ f x)



-- Task 8:
-- Suppose that we have a number of computations of type
-- "IO (Either error Unit)", which when run may yield either
-- the result "Right ()" if they compile normally or else
-- "Left e" where e is an element of some type "error", if
-- something goes wrong. Write a function that takes a list
-- of such computations and returns a computation that tries
-- to run them in order, but stops if it encounters an error, 
-- returning the error and discarding any pending computations
-- from the list
tryIOs : List (IO (Either error Unit)) -> IO (Maybe error)
tryIOs [] = pure Nothing
tryIOs (x :: xs) = do
                   result <- x
                   case result of
                     Left e => pure $ Just e
                     Right () => tryIOs xs



-- Task 9:
-- Suppose that we again want to run our list of computations
-- in order, but now we want to run them all unconditionally
-- and return a list of any errors that occurred
batchIOs : List (IO (Either error Unit)) -> IO (List error)
batchIOs [] = pure []
batchIOs (x :: xs) = do
                     result <- x
                     case result of
                       Left e => map (e ::) (batchIOs xs)
                       Right () => batchIOs xs



-- Test
goodComp : IO (Either String Unit)
goodComp = pure $ Right ()

errore : IO (Either String Unit)
errore = pure $ Left "Aaaa"

orrore : IO (Either String Unit)
orrore = pure $ Left "Bbbb"

prova : List (IO (Either String Unit))
prova = [goodComp, goodComp, errore, goodComp, orrore, goodComp]
