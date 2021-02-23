import Data.Vect

-- Exercise 1:

-- (a) Write a function "swapPair : (a, b) -> (b, a)"
swapPair : (a, b) -> (b, a)
swapPair (x, y) = (y, x)

-- (b) Write a function "swapEither : (Either a b) -> (Either b a)"
swapEither : (Either a b) -> (Either b a)
swapEither (Left x) = (Right x)
swapEither (Right x) = (Left x)

-- (c) Recall the identity function "id : a -> a" defined by "id x = x"
-- Define functions: "there : Nat -> List Unit" and "back : List Unit -> Nat"
-- such that for every "x : Nat" "back (there x) = id x : Nat"
-- and for every "y : List Unit" "there (back y) = id y : List Unit"
there : Nat -> List Unit
there 0 = []
there (S k) = () :: there k

back : List Unit -> Nat
back [] = 0
back (x :: xs) = 1 + back xs
-- TODO: prove that these are isos

-- (d) Recall that "Fin n" is the type of natural numbers less than n.
-- Define a function "project : Fin n -> Nat" that returns
-- the number, forgetting the part about it being less than n.
project : Fin n -> Nat
project FZ = 0
project (FS x) = 1 + project x

-- (e) Write a function "listify : Vect n a -> List a". Your function
-- should map the input vector to this list containing the same elements
-- in the same order.
listify : Vect n a -> List a 
listify [] = []
listify (x :: xs) = x :: listify xs



-- Exercise 2:
-- The reverse of a list contains the same elements, in reverse order.
-- For example, the reverse of [1, 2, 3] is [3, 2, 1]

-- (a) Write a function "reverseList : List a -> List a" that reverses its argument
reverseList : List a -> List a
reverseList [] = []
reverseList (x :: xs) = reverseList xs ++ [x]

-- (b) Write a function "reverseVect" that reverses a vector in a similar way
-- reverseVect : Vect n a -> Vect n a
-- reverseVect [] = []
-- reverseVect (x :: xs) = reverseVect xs ++ [x]
-- Just WRONG

-- (c) TODO


-- Exercise 3:
-- Consider the following type:
data Tree : Type -> Type where
  Leaf : Tree a
  Node : Tree a -> a -> Tree a -> Tree a

-- (a) Write a function "size : Tree a -> Nat" that returns the number of
-- values stored in the given tree (leaves don't count)
size : Tree a -> Nat
size Leaf = 0
size (Node x y z) = 1 + size x + size z

-- (b) Write a function "depth : Tree a -> Nat" that returns the depth
-- of the tree (Leaves have depth 0)
depth : Tree a -> Nat
depth Leaf = 0
depth (Node x y z) = if ((depth x) < (depth z)) then (1 + depth z) else (1 + depth x)

-- (c) write a function "flatten : Tree a -> List a" that returns a list containing the elements of a tree
flatten : Tree a -> List a
flatten Leaf = []
flatten (Node x y z) = flatten x ++ [y] ++ flatten z
