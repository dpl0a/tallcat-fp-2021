monus : Nat -> Nat -> Nat
monus 0 _ = 0
monus k 0 = k
monus (S k) (S j) = monus k j
