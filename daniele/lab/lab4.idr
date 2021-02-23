-- Exercise 1:
-- Write two functions
-- "beep : (Pair a b -> c) -> (a -> b -> c)"
-- "boop : (a -> b -> c) -> (Pair a b -> c)"
-- s.t. for every "f : Pair a b -> c" the equation
-- "boop (beep f) = f" holds, and for every function
-- "g : a -> b -> c" the equation "beep (boop g) = g"
-- holds.
beep : (Pair a b -> c) -> (a -> b -> c)
beep f x y = f (x, y)

boop : (a -> b -> c) -> (Pair a b -> c)
boop f (a, b) = f a b



-- Exercise 2:
foldList : b   -> (a -> b -> b) -> List a -> b
foldList ni co [] = ni
foldList ni co (x :: xs) = co x (foldList ni co xs) -- this is foldr

-- (a) Write functions
-- "conjunction : Bool -> Bool -> Bool"
-- "disjunction : Bool -> Bool -> Bool"
-- that compute the logical conjunction and
-- disjunction, respectively, of their inputs
conjunction : Bool -> Bool -> Bool
conjunction False y = False
conjunction True y = y

disjunction : Bool -> Bool -> Bool
disjunction False y = y
disjunction True y = True

-- (b) Using "foldList", write a function
-- "conj : List Bool -> Bool" that returns
-- The logical conjunction of the input list
conj : List Bool -> Bool
conj = foldList True conjunction

-- (c) Using "foldList", write a function
-- "disj : List Bool -> Bool" that returns
-- The logical disjunction of the input list
disj : List Bool -> Bool
disj = foldList False disjunction

-- (d) Write the filter function for lists
-- "filterList : (a -> Bool) -> List a -> List a"
-- in terms of foldList
filterList : (a -> Bool) -> List a -> List a
filterList f = foldList [] (\x, xs => if f x then x :: xs else xs)



-- Exercise 3:
-- Recall the type of binary trees:
data Tree : Type -> Type where
  Leaf : Tree a
  Node : Tree a -> a -> Tree a -> Tree a
  
-- Test tree
alberollo : Tree Nat
alberollo = Node Leaf 3 (Node (Node Leaf 4 Leaf) 5 Leaf)
  
-- (a) Write the fold function for binary trees
foldTree : b -> (b -> a -> b -> b) -> Tree a -> b
foldTree le no Leaf = le
foldTree le no (Node x y z) = no (foldTree le no x) y (foldTree le no z)

-- (b) Use "foldTree" to write the map function for binary trees
mapTree : (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Leaf (\x, y, z => Node x (f y) z)

-- (c) Use "foldTree" to write a function "sumTree : Tree Nat -> Nat"
-- that sums the data in the input tree
sumTree : Tree Nat -> Nat
sumTree = foldTree 0 (\x, y, z => x + y + z)
