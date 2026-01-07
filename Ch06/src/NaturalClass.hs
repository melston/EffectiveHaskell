module NaturalClass where

class (Show n, Eq n) => Natural n where 
    add :: n -> n -> n
    multiply :: n -> n -> n 
    additiveIdentity :: n 
    multiplicativeIdentity :: n 

instance Natural Int where
    add = (+)
    multiply = (*) 
    additiveIdentity = 0 
    multiplicativeIdentity = 1 
