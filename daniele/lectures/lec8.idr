import Data.List
import Data.Vect

-- Polymorphism : No assumptions about the structure
-- of the types involved

my_id : a -> a
my_id x = x

zipVect : Vect n a -> Vect n b -> Vect n (Pair a b)
zipVect [] [] = []
zipVect (x :: xs) (y :: ys) = (x, y) :: (zip xs ys)

-- How generic can i be?
-- plus : a -> a -> a
-- plus x y = x + y
-- But idris complains between a is not a numeric type

-- So we need some assumptions on the types we're using!
plus : Num a => a -> a -> a
plus x y = x + y
-- Saying "Num a =>" in a type signature in idris is
-- like assuming that the type a comes with some operations
-- this is called an _interface_ in idris and a _type class_
-- in haskell. This idea is similar to abstract classes in OOP



-- Let's see some interfaces:



-- The Show interface:

-- interface Show a where
--   show : a -> String
-- This produces a string with the input value, i.e.
-- λΠ> show 2.0
-- "2.0" : String

-- Let's write an implementation for show for the Shape type:
data Shape = IsoTriangle Double Double 
             | Rectangle Double Double 
             | Circle Double
             
implementation Show Shape where
  show (IsoTriangle x y) = "Triangle with base " ++ show x ++ "and height " ++ show y
  show (Rectangle x y) = "Rectangle with base " ++ show x ++ "and height " ++ show y
  show (Circle x) = "Circle with radius " ++ show x


-- Using alternative implementations
implementation [KleenePurist] Show Nat where
  show Z = "Z"
  show (S k) = "S (" ++ show k ++ ")"
-- λΠ>  show @{KleenePurist} 5
-- "S (S (S (S (S (Z)))))" : String



-- Using an interface as a constraint:
compliment : Show a => a -> String
compliment x = "Oh what a nice " ++ show x ++ " you are."
-- λΠ> compliment pi
-- "Oh what a nice 3.141592653589793 you are." : String
-- λΠ> compliment (Circle 3)
-- "Oh what a nice Circle with radius 3.0 you are." : String

implementation [ListShow] Show a => Show (List a) where
  show [] = ""
  show (x :: xs) = show x ++ ", " ++ show@{ListShow} xs
-- This is ugly but let's roll with it



-- The Eq interface:
-- It describes those types where i'm allowed to check
-- for equality of its elements

-- interface Eq a where
--   (==) : a -> a -> Bool
--   (/=) : a -> a -> Bool

-- You need to only implement one, the other is inferred for you
-- x /= y = not (x == y)
-- x == y = not (x /= y)

implementation Eq Shape where
  (==) (IsoTriangle x z) (IsoTriangle y w) = (x == y) && (z == w)
  (==) (Rectangle x z) (Rectangle y w) = (x == y) && (z == w)
  (==) (Circle x) (Circle y) = (x == y)
  (==) _ _ = False

-- Using Eq interfaces to count occurrences in a list
multiplicity : Eq a => a -> List a -> Nat
multiplicity target = length . filter (== target)

-- Let's implement a custom version of equality for nats
implementation [NatEq] Eq Nat where
  Z == Z = True
  (S k) == (S j) = (==) @{NatEq} k j
  _ == _ = False



-- The Ord interface:

-- data Ordering = LT | EQ | GT

-- interface Eq a => Ord a where
--   compare : a -> a -> Ordering
-- This gives you lots of functions

 -- sort : Ord a => List a -> List a
 
-- A function that checks if an Ord List is sorted
checkSorted : Ord a => List a -> Bool 
checkSorted [] = True
checkSorted (x :: []) = True
checkSorted (x :: (y :: xs)) = (x <= y) && (checkSorted (y :: xs))



-- The Num interface:

-- TODO: finish writing down the definition of Num



-- Let's play with complex numbers:

data GaussianInteger : Type where
  Gauss : Integer -> Integer -> GaussianInteger
  
implementation Cast Integer GaussianInteger where
  cast x = Gauss x 0

implementation Show GaussianInteger where
  show (Gauss x y) = show x ++ " + " ++ show y ++ "i"
-- Todo copy the nice definition

implementation Num GaussianInteger where
  (+) (Gauss a b) (Gauss c d) = Gauss (a + c) (b + d)
  (*) (Gauss a b) (Gauss c d) = Gauss (a * c - b * d) (b * c + a * d)
  fromInteger n = cast n



-- Defining an interface:

interface Mag a where
  magnitude : a -> Double
  
sumofSquares : (Num a) => Vect k a -> a
sumofSquares = foldr (\x,y => x*x + y) 0

implementation (Num a, Cast a Double) => Mag (Vect k a) where
  magnitude = sqrt . cast . sumOfSquares where
                sumOfSquares : (Num a) => Vect k a -> a
                sumOfSquares = foldr (\x,y => x * x + y) 0

implementation Cast GaussianInteger (Vect 2 Integer) where
  cast (Gauss x y) = [x, y]

implementation Mag GaussianInteger where
  magnitude = magnitude . the (Vect 2 Integer) . cast
