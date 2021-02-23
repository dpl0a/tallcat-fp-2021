import Data.String
-- import System.File
-- IO in Idris

-- Functions in Idris are _pure_
-- + Don't have side effects
-- + Output depends only on inputs (to a function)
-- We don't always have that. How do we get around that?

-- While we can't have pure functions that do any IO...
-- We CAN have pure functions that output a DESCRIPTION
-- on a "program", which can do IO (and also execute pure functions)

-- The type (constructor) of program descriptions is:
-- "IO : Type -> Type"
-- (IO a) is the type of program descriptions that return a value
-- of type a


-- Idris provides some functions of type IO which will be the
-- building blocks of our programs
-- λΠ> :t getLine
-- getLine : IO String
-- λΠ> :t putStr
-- putStr : String -> IO ()
-- λΠ> :exec (putStr "hello, world!\n")

-- Program descriptions can be _sequenced_
-- λΠ> :t (>>=)
-- (>>=) : Monad m => m a -> (a -> m b) -> m b

-- For IO it's "(>>=) : IO a -> (a -> IO b) -> IO b"
echo : IO Unit -- IO ()
echo = getLine >>= putStrLn
-- (you need to run ":exec echo" in the REPL)

-- There's another useful building block:
-- λΠ> :t pure
-- pure : Applicative f => a -> f a
-- λΠ> :exec pure 5

-- How can we compile Idris executable?
-- Well, we need a main
main : IO Unit -- Same as IO ()
main = echo

-- You can compile it in the REPL by loading
-- your .idr file and write
-- ":c name_of_executable name_of_function_you_want_to_compile"
-- and you'll find the executable in the build directory
-- Also check this out https://idris2.readthedocs.io/en/latest/backends/index.html

-- Now to read a list natural numbers one at a time!
-- We'll need "parsePositive" from "Data.List"
parseNat : String -> Maybe Nat
parseNat = parsePositive

-- Remember dthat "putStr : String -> IO ()"
getNats : IO (List Nat)
getNats = putStr "Please Enter a Nat: " >>= (\dontcare =>
          getLine >>= (\input =>
          case (parseNat input) of
            Nothing => pure []
            (Just n) => getNats >>= (\rest =>
                                    pure (n :: rest))))
-- And then run in the REPL:
-- λΠ> :exec (getNats >>= printLn)

-- "do notation"
getNats' : IO (List Nat)
getNats' = do
           putStr "Please Enter a Nat: "
           input <- getLine
           case (parseNat input) of
             Nothing => pure []
             (Just n) => do
                         rest <- getNats'
                         pure (n :: rest)
-- That's syntactic sugar for what we did before

-- How do we do file IO? Let's import "System.File" 
-- (this is needed only in Idris 2? In Idris 1.3.3 this is build into the prelude)
-- and these functions:

--            name of file                   contents
-- readFile : String -> IO (Either FileError String)

--             name      text to write
-- writeFile : String -> String -> IO (Either FileError ())
