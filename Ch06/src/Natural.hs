module Natural where

data Natural a = Natural
    { equal :: a -> a -> Bool
    , add :: a -> a -> a
    , multiply :: a -> a -> a
    , additiveIdentity :: a 
    , multiplicativeIdentity :: a 
    , displayAsString :: a -> String
    }

intNatural :: Natural Int 
intNatural = Natural
    { equal = (==)
    , add = (+)
    , multiply = (*)
    , additiveIdentity = 0
    , multiplicativeIdentity = 1 
    , displayAsString = show
    }
