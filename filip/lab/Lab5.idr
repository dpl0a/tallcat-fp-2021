import Data.Strings
import System.File

parseNat : String -> Maybe Nat
parseNat = parsePositive


--Task1

getMaybeNat : IO (Maybe Nat)
getMaybeNat = do
    putStr "Please enter a Nat: "
    input <- getLine
    pure (parseNat input)

getNat : IO Nat
getNat = do
    x <- getMaybeNat
    case (x) of
        Nothing => pure 0
        (Just n) => pure n
       
insistNat : IO Nat
insistNat = do
    x <- getMaybeNat
    case (x) of
        Nothing => do 
            n <- insistNat
            pure n
        (Just n) => pure n

insistAdd: Nat -> IO Nat
insistAdd x = do 
    y <- insistNat
    pure (x + y)

addAfter: (IO Nat) -> Nat -> IO Nat
addAfter f x = do
    y <- f
    pure (x + y)

--Task2

consolidate : List (Maybe a) -> Maybe (List a)
consolidate [] = Just []
consolidate (Nothing :: xs) = Nothing
consolidate ((Just x) :: xs) = case consolidate xs of
                            Nothing => Nothing 
                            Just y => Just (x :: y)

clean : List(Maybe a) -> (List a)
clean [] = []
clean (Nothing :: xs) = clean xs
clean ((Just x) :: xs) = x :: clean xs

natsGet : IO (Maybe (List Nat))
natsGet = do
    input <- getLine
    pure $ consolidate $ map parseNat $ words input

tryNats : IO (List Nat)
tryNats = do
    input <- getLine
    pure $ clean $ map parseNat $ words input

--Task 3

getLines : IO (List String)
getLines = do
    input <- getLine
    case input of
        "done" => pure [] 
        s => do
            ss <- getLines 
            pure (s :: ss)

        
dictate : IO ()
dictate = do
    text <- getLines
    filename <- getLine
    case filename of
        "none" => pure ()
        s => do
            _ <- writeFile s (unlines text)
            pure ()