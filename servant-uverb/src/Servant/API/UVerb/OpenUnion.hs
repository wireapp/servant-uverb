-- | Type-level code for implementing and using 'UVerb'.
module Servant.API.UVerb.OpenUnion
( IsMember
, Contains
, Nat_(..)
, RIndex
, Elem
, UElem(..)
, Nubbed
, CheckElemIsMember
, NoElementError
, DuplicateElementError
)
where

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

type family Nubbed xs :: Bool where
  Nubbed '[] = 'True
  Nubbed (x ': xs) = If (Elem x xs) (Nubbed xs) 'False

-- | Check whether @a@ is in list.  This will throw nice errors if the element is not in the
-- list, or if there is a duplicate in the list.
type family CheckElemIsMember (a :: k) (as :: [k]) :: Constraint where
    CheckElemIsMember a as =
      If (Elem a as)
        (If (Nubbed as)
          (() :: Constraint)
          (TypeError (DuplicateElementError as)))
        (TypeError (NoElementError a as))

type NoElementError (r :: k) (rs :: [k]) =
          'Text "Expected one of:"
    ':$$: 'Text "    " ':<>: 'ShowType rs
    ':$$: 'Text "But got:"
    ':$$: 'Text "    " ':<>: 'ShowType r

type DuplicateElementError (rs :: [k]) =
          'Text "Duplicate element in list:"
    ':$$: 'Text "    " ':<>: 'ShowType rs
