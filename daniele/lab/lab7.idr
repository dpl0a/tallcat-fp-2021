data CoNat : Type where
  Zero : CoNat
  Succ : (n : Inf CoNat) -> CoNat

data CoList : Type -> Type where
  Nil : CoList a
  (::) : (x : a) -> (xs : Inf (CoList a)) -> CoList a
  
mapCoList : (a -> b) -> CoList a -> CoList b
mapCoList f [] = []
mapCoList f (x :: xs) = f x :: (mapCoList f xs)

nats' : CoList Nat
nats' = 0 :: mapCoList S nats'

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k) 

natStream : Stream Nat
natStream = nats_helper 0 where 
       nats_helper: Nat -> Stream Nat
       nats_helper k = k :: nats_helper (S k)
       
infinity : CoNat
infinity = Succ infinity

-- Task 1
length : CoList a -> CoNat
length [] = Zero
length (x :: xs) = Succ (length xs)

-- Task 2
drop : Nat -> CoList a -> CoList a
drop Z x = x
drop (S k) [] = []
drop (S k) (x :: xs) = drop k xs

-- Task 3
filter : (a -> Bool) -> CoList a -> CoList a
filter f [] = []
filter f (x :: xs) = if f x then x :: (filter f xs) else (filter f xs)
-- λΠ> filter isEven nats' 
-- this doesn't terminate

-- Task 4
zipStream : (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipStream f xs ys = (f (head xs) (head ys)) :: (zipStream f (tail xs) (tail ys))

-- Task 5
zipStreamList : (a -> b -> c) -> Stream a -> List b -> List c
zipStreamList f xs [] = []
zipStreamList f xs (x :: ys) = (f (head xs) x) :: (zipStreamList f (tail xs) ys)

-- Task 6
makePair : a -> b -> (a, b)
makePair x y = (x, y)

enumerate : List a -> List (Pair Nat a)
enumerate xs = zipStreamList makePair natStream xs

-- Task 7 
minus : CoNat -> CoNat -> CoNat
minus Zero y = Zero
minus (Succ n) Zero = Succ n
minus (Succ n) (Succ x) = minus n x

-- Task 8
TODO
