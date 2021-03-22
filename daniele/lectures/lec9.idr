import Data.Vect

-- Lecture 9: Algebraic interfaces

-- Running example: A data structure for (finite) sets
-- Recall - sets are collections of elements (of some type)
-- e.g. {0,1,2,3} : Set Nat
-- * Order doesn't matter {0,1} == {1,0}
-- * Repetition doesn't matter {1,1} == {1}
-- Two sets are equal precisely when they contain the same elements

infixr 10 \/ -- "\/" is an infix operator with a certain binding strength

data Set : Type -> Type where
  Empty : Set a
  (\/) : a -> Set a -> Set a
  
implementation Show a => Show (Set a) where
  show x = "{ " ++ comma_sep x ++ " }" where
    comma_sep Empty = ""
    comma_sep (x \/ Empty) = show x
    comma_sep (x \/ (y \/ z)) = show x ++ ", " ++ comma_sep (y \/ z)

-- Useful cast since we can steal list syntax
implementation Cast (List a) (Set a) where
  cast [] = Empty
  cast (x :: xs) = x \/ cast xs
  
-- Check if an element is in a set
isIn : Eq a => a -> Set a -> Bool
isIn _ Empty = False
isIn x (y \/ z) = if (x == y) then True else isIn x z

-- Check if all the elements of one set are in a second set
isSubset : Eq a => Set a -> Set a -> Bool
isSubset Empty y = True
isSubset (x \/ z) y = (isIn x y) && (isSubset z y)

-- Set equality
implementation Eq a => Eq (Set a) where
  xs == ys = (isSubset xs ys) && (isSubset ys xs)



-- The Semigroup interface:
-- interface Semigroup ty where
--   (<+>) : ty -> ty -> ty
-- <+> is assumed to be *associative*

-- Set union
setUnion : Set a -> Set a -> Set a
setUnion Empty y = y
setUnion (x \/ z) y = x \/ setUnion z y

implementation Semigroup (Set a) where
  (<+>) = setUnion



-- The Monoid interface:
-- interface Semigroup ty => Monoid ty where
--   neutral : ty
-- With the assumptions that neutral is the 
-- identity element for <+>

implementation Monoid (Set a) where
  neutral = Empty



-- The Functor interface:
-- interface Functor (f : Type -> Type) where
--   map : (m : a -> b) -> f a -> f b

-- interface Functor List where
--   map f [] = []
--   map f (x :: xs) = f x :: map f xs

-- Let's give a different definition of trees:
data Tree : Type -> Type where
  Leaf : (label : a) -> Tree a
  Node : (label : a) -> (leftChild : Tree a) -> (rightChild : Tree a) -> Tree a
  
-- Pretty printing for Tree
implementation Show a => Show (Tree a) where
  show (Leaf label) = "[" ++ show label ++ "]"
  show (Node label leftChild rightChild) = "[" ++ show leftChild ++ show label ++ show rightChild ++ "]"

implementation Functor Tree where
  map f (Leaf label) = Leaf (f label)
  map f (Node label leftChild rightChild) = Node (f label) (map f leftChild) (map f rightChild)

-- Let's return to sets:
implementation Functor Set where
  map f Empty = Empty
  map f (x \/ y) = (f x) \/ (map f y)



-- The Applicative interface
-- interface Functor f => Applicative (f : Type -> Type) where
--   pure : a -> fa
--   (<*>) : f (a -> b) -> f a -> f b
-- Where (<*>) should be thougt of a kind of generalised function application

implementation [MyMaybeAppl] Applicative Maybe where
  pure x = Just x
  (Just f) <*> (Just x) = Just (f x)
  _ <*> _ = Nothing

-- Is there a reasonable way to implement the Applicative interface for Sets?
implementation Applicative Set where
  pure a = a \/ Empty
  (f \/ fs) <*> xs = setUnion (map f xs) (fs <*> xs)
  Empty <*> _ = Empty



-- The Monad interface:
-- interface Applicative m => Monad (m : Type -> Type) where
--   (>>=) : m a -> (a -> m b) -> m b

implementation [MyMaybeMonad] Monad Maybe where
  Nothing >>= f = Nothing
  (Just x) >>= f = f x

-- Let's see "bind" in practice:
maybeAdd : (Num a) => Maybe a -> Maybe a -> Maybe a
maybeAdd m n = m >>= (\x => (n >>= (\y => pure (x + y))))
-- Kinda ugly, let's use the do notation:

maybeAddDo : (Num a) => Maybe a -> Maybe a -> Maybe a
maybeAddDo m n = do
                 x <- m
                 y <- n
                 pure (x + y)

-- Can we make Set into a Monad?
implementation Monad Set where
  Empty >>= f = Empty
  (x \/ xs) >>= f = setUnion (f x) (xs >>= f)

intersection : Eq a => Set a -> Set a -> Set a
intersection sx sy = do
                     x <- sx
                     y <- sy
                     if (x == y) then pure x else Empty

cartesianProduct : Set a -> Set b -> Set (Pair a b)
cartesianProduct sx sy = do
                         x <- sx
                         y <- sy
                         pure (x, y)
                         
canonicalSeq : Monad m => m (a -> b) -> m a -> m b
canonicalSeq mf mx = do
                     f <- mf
                     x <- mx
                     pure (f x)
