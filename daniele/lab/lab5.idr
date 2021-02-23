import Data.String
-- I'm using Idris 1.3.3 so i don't need to import System.File

-- We also need the following definition
parseNat : String -> Maybe Nat
parseNat = parsePositive



-- Exercise 1:

-- (a) Write a function "getNat : IO Nat" which, when
-- executed, prompts the user to enter a natural number,
-- the program should return that number.
-- Otherwise, the program should return zero.
getNat : IO Nat
getNat = do
         putStr "Please Enter a Nat: "
         input <- getLine
         case (parseNat input) of
           Nothing => pure 0
           Just x => pure x

-- (b) Write a function "insistNat : IO Nat" which,
-- when executed, prompts the user to enter a natural
-- number. If the user enters a valid natural number,
-- the program should return that number.
-- Otherwise, the program should prompt the user
-- again, until a valid natural number is entered.
insistNat : IO Nat
insistNat = do
            putStr "Please Enter a Nat: "
            input <- getLine
            case (parseNat input) of
              Nothing => insistNat
              Just x => pure x

-- (c) Write a function "insistAdd : Nat -> IO Nat"
-- s.t. for "n : Nat" the program "insistAdd n"m when
-- executed, prompts the user for a natural number
-- until a valid one is entered (use insistNat),
-- and then returns the result of adding n to
-- that number
insistAdd : Nat -> IO Nat
insistAdd k = map (+ k) insistNat

-- (d) Write a function "addAfter : (IO Nat) -> Nat -> IO Nat"
-- s.t. "addAfter insistNat" does the same thing as insistAdd.
-- How does the behaviour of "addAfter insistNat" differ from
-- the behaviour of "addAfter getNat"? Give an example
-- where they differ.
addAfter : (IO Nat) -> Nat -> IO Nat
addAfter x k = map (+ k) x
-- They differ when the first input is invalid, e.g.
-- λΠ> :exec (addAfter getNat 3 >>= printLn)
-- Please Enter a Nat: asdf
-- 3
-- This returned "0 + 3" because the function
-- "getNat" returns "0" on an invalid input
-- While, if we try executing: 
-- λΠ> :exec (addAfter insistNat 3 >>= printLn)
-- Please Enter a Nat: asdf
-- Please Enter a Nat: 10
-- 13
-- Because the function "insistNat" keeps asking
-- for a natural number until a valid natural "x" is
-- entered, then returns "x + 3" 



-- Exercise 2:

-- Let's write some helper functions
consolidate : List (Maybe a) -> Maybe (List a)
consolidate [] = Just []
consolidate (Nothing :: xs) = Nothing
consolidate ((Just x) :: xs) = case consolidate xs of
                                 Nothing => Nothing
                                 Just y => Just (x :: y)

realize : List (Maybe a) -> List a 
realize [] = []
realize (Nothing :: xs) = realize xs
realize ((Just x) :: xs) = x :: realize xs

-- (a) Write a function "natsGet : IO (Maybe (List Nat))"
-- that reads a line of user input consisting of space
-- separated natural numbers, and returns the corresponding
-- "List Nat". If the user input cannot be parsed as a
-- list of natural numbers, the program should return "Nothing"
natsGet : IO (Maybe (List Nat))
natsGet = do
          input <- getLine
          pure $ consolidate $ map parseNat $ words input
          
-- (b) Write a function "tryNats : IO (List Nat)"
-- that reads a line of user input consisting of
-- space separated natural numbers, and returns
-- the corresponding "List Nat". If some part of
-- the user input cannot be parsed as a natural
-- number, it should be omitted from the list, but
-- the rest of the list shoudl still be returned
tryNats : IO (List Nat)
tryNats = do
          input <- getLine
          pure $ realize $ map parseNat $ words input



-- Exercise 3:

-- (a) Write a function "getLines : IO (List String)"
-- that reads lines of user input until the user enters
-- "done", and returns the lines as a "List String"
getLines : IO (List String)
getLines = do
           putStr "Enter Line: "
           input <- getLine
           case input of
             "done" => pure []
             line => do
                      rest <- getLines
                      pure $ input :: rest
 
-- (b) Write a function "dictate : IO ()" and compile
-- it to obtain an executable. When run, "dictate" should
-- read lines until the user enters "done", then prompt
-- the user for the name of a file to store those lines in.
-- If the user enters "non", the lines are instead thrown
-- away. If the user enters a valid file name, then the
-- program should attempt to store the lines of user 
-- input as a file with that name. A message realying
-- the success or failure of this operation should be
-- be printed before the program exits.
dictate : IO ()
dictate = do
          input <- getLines
          putStr "Enter Storage Location: "
          location <- getLine
          case location of
            "none" => putStrLn "Throwing Lines Away!"
            loca => do
                    writeFile location $ unlines input
                    putStrLn "Success!"
