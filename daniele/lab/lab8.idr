import Data.Vect
import Data.List

data GaussianInteger : Type where
  Gauss : Integer -> Integer -> GaussianInteger
  
implementation Cast Integer GaussianInteger where
  cast x = Gauss x 0
  
implementation Show GaussianInteger where
  show (Gauss x y) = show x ++ complex_part y where
    complex_part : Integer -> String
    complex_part x = case x of
                      0 => ""
                      otherwise => (if x > 0 then " + " else " - ")
                                      ++ (if (x /= 1) && (x /= -1) then show (abs x) else "")
                                        ++ "i"

implementation Num GaussianInteger where
  (+) (Gauss a b) (Gauss c d) = Gauss (a + c) (b + d)
  (*) (Gauss a b) (Gauss c d) = Gauss (a * c - b * d) (b * c + a * d)
  fromInteger n = cast n
  
interface Mag a where
  magnitude : a -> Double

implementation Cast GaussianInteger (Vect 2 Integer) where
  cast (Gauss x y) = [x, y]
      
implementation (Num a, Cast a Double) => Mag (Vect k a) where
  magnitude = sqrt . cast . sumOfSquares where
                sumOfSquares : (Num a) => Vect k a -> a
                sumOfSquares = foldr (\x,y => x * x + y) 0
      
implementation Mag GaussianInteger where
  magnitude = magnitude . the (Vect 2 Integer) . cast
  
  
  
-- Task 1:
-- Write a "Neg" instance for the type of Gaussian integers
implementation Neg GaussianInteger where
  negate (Gauss x y) = Gauss (- x) (- y)
  (-) (Gauss x z) (Gauss y w) = Gauss (x - y) (z - w)



-- Task 2:
-- Write an "Eq" instance for Gaussian integers:
implementation Eq GaussianInteger where
  (==) (Gauss x z) (Gauss y w) = if (x == y) && (z == w) then True else False



-- Task 3:
-- Write a named "Ord" instance for Gaussian integers
-- that compares them lexicographically:
-- > compare @{lex} (Gauss 1 200) (Gauss 2 1)
-- LT
-- > compare @{lex} (Gauss 2 1) (Gauss 2 1)
-- EQ
-- > compare @{lex} (Gauss 3 1) (Gauss 2 4)
-- GT
implementation [lex] Ord GaussianInteger where
  compare (Gauss x z) (Gauss y w) = case compare x y of
                                      GT => GT
                                      LT => LT
                                      EQ => case compare z w of
                                              GT => GT
                                              LT => LT
                                              EQ => EQ

-- Task 4:
-- Use the "Mag" instance for Gaussian integers defined in the lecture
-- to write a named "Ord" instance for Gaussian integers which compares
-- them by magnitude:
-- > compare @{mag} (Gauss 1 200) (Gauss 2 1)
-- GT
-- > compare @{mag} (Gauss 2 1) (Gauss 2 1)
-- EQ
-- > compare @{mag} (Gauss 3 1) (Gauss 2 4)
-- LT
implementation [mag] Ord GaussianInteger where
  compare x y = compare (magnitude x) (magnitude y)



-- Task 5:
-- Write a named "Eq" instance for lists that compares them _setwise_,
-- that is, two lists should be considered equal if each element that 
-- occurs (at least once) in one ofthe lists also occurs 
-- (at least once) in the other:
-- > (==) @{setwise} [1,2,3] [3,2,1]
-- True
-- > (==) @{setwise} [1,2,3] [1,2,3,3]
-- True
-- > (==) @{setwise} [1,2,3] [1,2,4]
-- False
implementation [setwise] Eq a => Eq (List a) where
  (==) xs ys = (all ((flip elem) xs) ys) && (all ((flip elem) ys) xs)



-- Task 6:
-- Write a named Eq instance for lists that compares them _multisetwise_,
-- that is, two lists should be considered equal if each list contains
-- the same number of copies of eachelement as the other,
-- regardless of order:
-- > (==) @{multisetwise} [1,2,3] [3,2,1]
-- True
-- > (==) @{multisetwise} [1,2,3] [1,2,3,3]
-- False
-- > (==) @{multisetwise} [1,2,3] [1,2,4]
-- False
implementation [multisetwise] Eq a => Eq (List a) where
  (==) [] [] = True
  (==) [] _ = False
  (==) (x :: xs) ys = if elem x ys then (xs == (delete x ys)) else False
-- TODO: Write a prettier one
