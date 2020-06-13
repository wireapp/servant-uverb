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
import Servant.API (StdMethod(..), JSON)
import Servant.Server
import GHC.TypeLits
import Data.SOP.NS
import Data.Proxy

-- * Servant.API.Status
class KnownNat n => KnownStatus n where
  statusVal :: proxy n -> Status


class  KnownStatus (TheStatus a) => HasStatus (a :: *) where
  type TheStatus (a :: *) :: Nat

instance KnownStatus 203 where
  statusVal = const status203


status :: forall a proxy. HasStatus a => proxy a -> Status
status = const (statusVal (Proxy :: Proxy (TheStatus a)))

newtype WithStatus (k :: Nat) a = WithStatus a

instance KnownStatus n => HasStatus (WithStatus n a) where
  type TheStatus (WithStatus n a) = n

--


-- * Servant.API.UVerb
data UVerb (method :: StdMethod) (cts :: [*]) (resources :: [*])

-----------

data FisxUser = FisxUser { name :: String }

instance HasStatus FisxUser where
  type TheStatus FisxUser = 203

type FisxAPI = UVerb 'GET '[JSON] '[FisxUser]

data ArianUser = ArianUser

type ArianAPI = UVerb 'GET '[JSON] '[WithStatus 201 ArianUser]



{-
instance  HasServer (Get ctypes xs) where
  type Server = Sum (Server xs)

  server :: All HasStatus xs => IO (Sum (Server xs)) -> IO ()
  server handler = do
    xs <- handler
    status <- mapSum (\(HasStatus a => x :: a) -> knownNatVal (Proxy :: Proxy @(TheStatus a)) xS
    respond status


-}


main = undefined

-- handler' :: Server API'  -- ~  IO (NS I '[WithStatus 500 Bool, WithStatus 403 Bool])
-- handler' = pure $ injectNS $ WithStatus' "error"


