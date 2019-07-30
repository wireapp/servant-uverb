-- | Type-level code for implementing and using 'UVerb'.
module Servant.OpenUnion where

import Data.SOP.Constraint
import Data.SOP.NS
import GHC.TypeLits
import Servant.API (If)


-- * Stuff stolen from 'Data.WorldPeace" but for generics-sop

type IsMember (a :: u) (as :: [u]) = (CheckElemIsMember a as, UElem a as (RIndex a as))

type family Contains (as :: [k]) (bs :: [k]) :: Constraint where
  Contains '[] _ = ()
  Contains (a ': as) bs = (IsMember a bs, Contains as bs)

data Nat_ = S_ Nat_ | Z_


-- | TODO: we probably can make do without this.
type family RIndex (r :: k) (rs :: [k]) :: Nat_ where
  RIndex r (r ': rs) = 'Z_
  RIndex r (s ': rs) = 'S_ (RIndex r rs)

type family Elem (x :: k) (xs :: [k]) :: Bool where
    Elem _ '[]       = 'False
    Elem x (x ': xs) = 'True
    Elem x (y ': xs) = Elem x xs


class i ~ RIndex a as => UElem (a :: k) (as :: [k]) (i :: Nat_) where
  inject :: f a -> NS f as
  match  :: NS f as -> Maybe (f a)

instance UElem a (a ': as) 'Z_ where
  inject x = Z x
  match y = case y of
    Z x -> Just x
    _ -> Nothing

instance ( RIndex a (b ': as) ~ ('S_ i) , UElem a as i) => UElem a (b ': as) ('S_ i) where
  inject x = S (inject x)
  match y = case y of
    Z _ -> Nothing
    S z -> match z


-- | Check whether @a@ is in list.
type family CheckElemIsMember (a :: k) (as :: [k]) :: Constraint where
    CheckElemIsMember a as =
      If (Elem a as) (() :: Constraint) (TypeError (NoElementError a as))

type NoElementError (r :: k) (rs :: [k]) =
          'Text "Expected one of:"
    ':$$: 'Text "    " ':<>: 'ShowType rs
    ':$$: 'Text "But got:"
    ':$$: 'Text "    " ':<>: 'ShowType r


-- * Stolen from "Servant.Swagger.Internal.TypeLevel.API"

-- | Remove duplicates from a type-level list.
type family Nub xs where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (Remove x xs)

-- | Remove element from a type-level list.
type family Remove x xs where
  Remove x '[]       = '[]
  Remove x (x ': ys) =      Remove x ys
  Remove x (y ': ys) = y ': Remove x ys
