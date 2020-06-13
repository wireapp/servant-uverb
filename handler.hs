{-# LANGUAGE ConstraintKinds
, DataKinds
, DeriveFunctor
, DeriveGeneric
, DerivingStrategies
, DerivingVia
, DuplicateRecordFields
, FlexibleContexts
, FlexibleInstances
, GeneralizedNewtypeDeriving
, InstanceSigs
, KindSignatures
, LambdaCase
, MultiParamTypeClasses
, OverloadedStrings
, PolyKinds
, RankNTypes
, RecordWildCards
, ScopedTypeVariables
, StandaloneDeriving
, TupleSections
, TypeApplications
, TypeOperators
, TypeFamilies
, UndecidableInstances
, ViewPatterns
#-}

import Network.HTTP.Types(Status, status203)
import Servant.API (StdMethod(..), JSON, (:<|>), (:>))
import Servant.Server
import GHC.TypeLits
import Data.SOP.NS
import Control.Monad.Identity
import Data.Proxy
import Servant.API.UVerb.OpenUnion


-- * Servant.API.Status
--

class KnownNat n => KnownStatus n where
  statusVal :: proxy n -> Status

instance KnownStatus 203 where
  statusVal = const status203


-- * Servant.API.UVerb
--

class  KnownStatus (StatusOf a) => HasStatus (a :: *) where
  type StatusOf (a :: *) :: Nat

status :: forall a proxy. HasStatus a => proxy a -> Status
status = const (statusVal (Proxy :: Proxy (StatusOf a)))

newtype WithStatus (k :: Nat) a = WithStatus a

instance KnownStatus n => HasStatus (WithStatus n a) where
  type StatusOf (WithStatus n a) = n

-- FUTUREWORK:
-- @type Verb method statusCode contentTypes a = UVerb method contentTypes [WithStatus statusCode a]@
data UVerb (method :: StdMethod) (contentTypes :: [*]) (as :: [*])


-- * Servant.API.UVerb
--

-- | 'return' for 'UVerb' handlers.  Takes a value of any of the members of the open union,
-- and will construct a union value in an 'Applicative' (eg. 'Server').
respond
  :: forall (f :: * -> *) (x :: *) (xs :: [*]). (Applicative f, HasStatus x, IsMember x xs)
  => x -> f (NS Identity xs)
respond = pure . inject . Identity




-- * example use case
--

data FisxUser = FisxUser { name :: String }

-- | we can get around 'WithStatus' if we want to, and associate the status code with our
-- resource types directly.
--
-- (to avoid orphan instances and make it more explicit what's in the API and what isn't, we
-- could introduce a newtype 'Resource' that wraps all the types we're using in our routing
-- table, and then define lots of 'HasStatus' instances for @Resource This@ and @Resource
-- That@.)
instance HasStatus FisxUser where
  type StatusOf FisxUser = 203

data ArianUser = ArianUser

type API = "fisx" :> UVerb 'GET '[JSON] '[FisxUser, WithStatus 303 String]
      :<|> "arian" :> UVerb 'GET '[JSON] '[WithStatus 201 ArianUser]



{-
instance  HasServer (Get ctypes xs) where
  type Server = Sum (Server xs)

  server :: All HasStatus xs => IO (Sum (Server xs)) -> IO ()
  server handler = do
    xs <- handler
    status <- mapSum (\(HasStatus a => x :: a) -> knownNatVal (Proxy :: Proxy @(TheStatus a)) xS
    respond status


-}


main :: IO ()
main = undefined

handler :: Server API
handler = undefined
