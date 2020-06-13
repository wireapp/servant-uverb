{-# LANGUAGE ConstraintKinds
, DataKinds
, InstanceSigs
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

{-# OPTIONS_GHC -Wno-unused-imports #-}

import Network.HTTP.Types
import System.Timeout
import Servant.API (StdMethod(..), JSON, (:<|>)((:<|>)), (:>), Capture)
import Servant.Server
import Data.ByteString (ByteString)
import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Data.SOP.NS
import Control.Monad.Identity
import Data.Proxy
import Servant.API.UVerb.OpenUnion
import qualified Network.Wai.Handler.Warp as Warp

import Data.Maybe (fromMaybe)
import qualified Network.Wai as Wai
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.NS
import Data.String.Conversions
import Network.HTTP.Types (Status, hContentType, hAccept)
import Network.Wai (requestHeaders, responseLBS)
import Servant.API.ContentTypes
import Servant (ReflectMethod, reflectMethod)
import Servant.Server.Internal


-- * Servant.API.Status
--

class KnownNat n => KnownStatus n where
  statusVal :: proxy n -> Status

instance KnownStatus 203 where
  statusVal = const status203

instance KnownStatus 201 where
  statusVal = const status201

instance KnownStatus 303 where
  statusVal = const status303


-- * Servant.API.UVerb
--

class KnownStatus (StatusOf a) => HasStatus (a :: *) where
  type StatusOf (a :: *) :: Nat

statusOf :: forall a proxy. HasStatus a => proxy a -> Status
statusOf = const (statusVal (Proxy :: Proxy (StatusOf a)))

newtype WithStatus (k :: Nat) a = WithStatus a
  deriving (Generic)

instance KnownStatus n => HasStatus (WithStatus n a) where
  type StatusOf (WithStatus n a) = n

-- FUTUREWORK:
-- @type Verb method statusCode contentTypes a = UVerb method contentTypes [WithStatus statusCode a]@
data UVerb (method :: StdMethod) (contentTypes :: [*]) (as :: [*])

type Union = NS Identity


-- * Servant.API.UVerb
--

-- | 'return' for 'UVerb' handlers.  Takes a value of any of the members of the open union,
-- and will construct a union value in an 'Applicative' (eg. 'Server').
respond
  :: forall (x :: *) (xs :: [*]) (f :: * -> *). (Applicative f, HasStatus x, IsMember x xs)
  => x -> f (Union xs)
respond = pure . inject . Identity

-- | Helper constraint used in @instance 'HasServer' 'UVerb'@.
type IsResource contentTypes = AllCTRender contentTypes `And` HasStatus

instance
  ( ReflectMethod method
  , AllMime contentTypes
  , All (IsResource contentTypes) as
  ) => HasServer (UVerb method contentTypes as) context where

  type ServerT (UVerb method contentTypes as) m = m (Union as)

  hoistServerWithContext _ _ nt s = nt s

  route :: forall env. Proxy (UVerb method contentTypes as)
                                    -> Context context
                                    -> Delayed env (Server (UVerb method contentTypes as))
                                    -> Router env
  route _proxy _ctx action = leafRouter route'
    where
      method = reflectMethod (Proxy @method)

      route' env request cont = do
        let accH :: ByteString  -- for picking the content type.
            accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request

            action' :: Delayed env (Handler (Union as))
            action' = action
                `addMethodCheck` methodCheck method request
                `addAcceptCheck` acceptCheck (Proxy @contentTypes) accH

            mkProxy :: a -> Proxy a
            mkProxy _ = Proxy

        runAction action' env request cont $ \(output :: Union as) -> do
          let encodeResource :: (AllCTRender contentTypes a, HasStatus a) => Identity a -> K (Status, Maybe (LBS, LBS)) a
              encodeResource (Identity res) = K
                  ( statusOf $ mkProxy res
                  , handleAcceptH (Proxy @contentTypes) (AcceptHeader accH) res
                  )

              pickResource :: Union as -> (Status, Maybe (LBS, LBS))
              pickResource = collapse_NS . cmap_NS (Proxy @(IsResource contentTypes)) encodeResource

          case pickResource output of
            (_, Nothing) -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
            (status, Just (contentT, body)) ->
              let bdy = if allowedMethodHead method request then "" else body
              in Route $ responseLBS status ((hContentType, cs contentT) : []) bdy


-- * example use case
--

data FisxUser = FisxUser { name :: String }
  deriving (Generic)

instance ToJSON FisxUser

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
  deriving Generic

instance ToJSON ArianUser

instance (Generic (WithStatus n a), ToJSON a) => ToJSON (WithStatus n a)

type API = "fisx" :> Capture "bool" Bool :> UVerb 'GET '[JSON] '[FisxUser, WithStatus 303 String]
      :<|> "arian" :> UVerb 'GET '[JSON] '[WithStatus 201 ArianUser]

fisx :: Bool -> Handler (Union '[FisxUser, WithStatus 303 String])
fisx True = respond (FisxUser "fisx")
fisx False = respond (WithStatus @303 ("still fisx" :: String))

arian :: Handler (Union '[WithStatus 201 ArianUser])
arian = respond (WithStatus @201 ArianUser)

handler :: Server API
handler = fisx :<|> arian

main :: IO ()
main = void . timeout 10000000 . Warp.run 8080 $ serve (Proxy @API) handler
