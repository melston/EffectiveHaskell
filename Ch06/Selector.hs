{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Selector where
import Data.Kind

class Select (f :: Type -> Type) where
    empty :: f a
    pick :: f a -> f a -> f a

instance Select Maybe where
    empty = Nothing
    pick Nothing a = a
    pick a _ = a

instance Select [] where
    empty = []
    pick = (<>)


-----------------------------------

newtype Sel (f :: Type -> Type) (a :: Type) = Sel (f a)
    deriving Show

-------------------------------------

instance (Select f) => Semigroup (Sel f a) where
    (Sel a) <> (Sel b) = Sel (pick a b)

instance (Select f) => Monoid (Sel f a) where
    mempty = Sel empty

---------------------------------------

-- Use DerivingVia to derive Semigroup and Monoid for MyMaybe
-- from the 'representationally equal' type Sel Maybe a.
newtype MyMabye a = MyMaybe (Maybe  a)
    deriving Show
    deriving (Semigroup, Monoid) via (Sel Maybe a)


