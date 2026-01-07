{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TypeApplications #-}
module TypeApplicationsDemo where

adheresToReadShowContract :: forall a. (Read a, Show a) => a -> Bool
adheresToReadShowContract val =
    let a = show . read @a . show $ val
        b = show val 
    in a == b

showLeftRight :: (Read a, Read b) => String -> Either a b 
showLeftRight s
    | length s > 5 = Left (read s) 
    | otherwise = Right (read s)
