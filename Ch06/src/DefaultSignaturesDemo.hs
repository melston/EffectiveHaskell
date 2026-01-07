{-# LANGUAGE DefaultSignatures, TypeApplications #-}
module DefaultSignaturesDemo where

-- No requirement for Show instance if we provide our own implementation of Show
class Redacted a where 
    redacted :: a -> String
    default redacted :: Show a => a -> String
    redacted = show

data UserName = UserName String

instance Show UserName where 
    show (UserName name) = name

instance Redacted UserName where 

data Password = Password String

instance Redacted Password where 
    redacted _ = "<REDACTED>"
