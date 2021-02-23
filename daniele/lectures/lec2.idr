-- You can define types with "data" and you have to tell idris it's a type with ": Type"
data One : Type where
  O : One

-- Let's write some functions using our newly defined type
id_one : One -> One
id_one O = O

-- Pretty boring, huh? Well... That's the only function of type "One -> One"
-- Let's define another type

data Zero : Type where
  -- Aaaaand it's empty

-- But we can still define functions from Zero
what_one : Zero -> One
what_one x impossible

-- What's after Zero and One? Two!
data Two : Type where
  Tru : Two
  Fal : Two
  
-- MOAR FUNCTIONS
fun_t : One -> Two
fun_t O = Tru

fun_ : One -> Two
fun_ O = Fal

-- That's all the functions "One -> Two"... Still pretty boring!
-- Let's define one of the most stupid functions ever

-- nope : One -> Zero
-- nope o = 
-- Yyyyeah, just nope... We just can't define this

-- Two is a pretty useful type, take a look:
-- branch : Two -> Nat -> Nat -> Nat
-- branch t k j = k
-- branch f k j = j
-- This doesn't work, why? Because we used lowcase constructor names for Two and this fucks pattern matching up, don't do it.

-- By the way in idris we don't use Two, but Bool... Let's try again:
branch : Bool -> Nat -> Nat -> Nat
branch x a b = if x then a else b
-- Okay, this one works.

-- We have all the common opertions on booleans like (&&), (||), etc

-- We can also check for equality! (ask for :doc (==) in the REPL)

-- Let's define our first serious type: the natural numbers 
-- data N : Type where
--  Z : N -- 0 is a natural number
--  S : N -> N -- if n is a natural number, then S is a natural number
-- Yeah! Peano style!

{-
0 ~~> Z
1 ~~> S 0
2 ~~> S (S 0)
etc

You don't need to put brackets around stuff, no LISP-ing allowed
-}

-- Let's use idris' naturals "Nat" to define addition:
add : Nat -> Nat -> Nat
add Z     j = j
add (S k) j = S (add k j)

-- And now for multiplication:
mul : Nat -> Nat -> Nat
mul Z j = Z
mul (S k) j = add (mul k j) j

-- Let's do the factorial:
fac : Nat -> Nat
fac Z = 1
fac (S k) = mul (S k) (fac k)

-- Let's talk about shapes:
data Shape : Type where
  Circle : Nat -> Shape -- (Circle k) is the circle of radius k
  Rectangle : Nat -> Nat -> Shape -- (Rectangle a b) is the rectangle of length a and width b
  IsoTriangle : Nat -> Nat -> Shape -- (IsoTriangle b a) is the triangle with base of len b and other two sides of len a
  
-- Now let's define a perimeter function:
--perimeter : Shape -> Nat
--perimeter (Circle k) = mul (mul 2 k) pi
--perimeter (Rectangle k j) = ?perimeter_rhs_

-- This complains because the multiplication returns a double, let's write a *correct* version:
perimeter : Shape -> Double
perimeter (Circle k) = 2 * pi * (cast k) -- We're casting a natural to double
perimeter (Rectangle k j) = cast ((2 * k) + (2 * j))
perimeter (IsoTriangle k j) = cast (k + (2 * j))

-- Let's try it by defining a shape:
r1 : Shape
r1 = Rectangle 5 6

-- We're lazy and we want a shortcut for squares:
square : Nat -> Shape
square k = Rectangle k k

-- Let's define another (not so useful) function:
fun_shape : Nat -> Nat -> Shape
fun_shape k j = if k == j then (Circle k) else (Rectangle k j)
