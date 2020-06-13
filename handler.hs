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
import Servant.API (StdMethod(..), JSON, (:<|>)((:<|>)), (:>), Capture)
import Servant.Server
import Data.ByteString (ByteString)
import Data.Aeson
import qualified GHC.Generics as GHC
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

import qualified Data.ByteString.Lazy as LB
import Data.Foldable (toList)
import Data.Proxy (Proxy(Proxy))
import Data.SOP.BasicFunctors ((:.:)(Comp))
import Data.SOP.Constraint(All, And, Compose)

import Data.SOP.NP (NP(..), cpure_NP)
import Servant.API.ContentTypes (MimeUnrender(mimeUnrender), Accept, contentTypes)
import Servant.API (ReflectMethod(reflectMethod))
import Servant.Client.Core (ClientError(..), HasClient(Client, hoistClientMonad, clientWithRoute), RunClient(..), runRequest, requestMethod, responseStatusCode, responseBody, requestAccept)

import Servant.Client.Core.Response
import Network.HTTP.Media (MediaType, parseAccept, (//))

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Network.HTTP.Media                   (matches)
import           Network.HTTP.Types (Status)
import Control.Monad (unless)



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
  deriving (GHC.Generic)

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
type IsServerResource contentTypes = AllCTRender contentTypes `And` HasStatus

instance
  ( ReflectMethod method
  , AllMime contentTypes
  , All (IsServerResource contentTypes) as
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
              pickResource = collapse_NS . cmap_NS (Proxy @(IsServerResource contentTypes)) encodeResource

          case pickResource output of
            (_, Nothing) -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
            (status, Just (contentT, body)) ->
              let bdy = if allowedMethodHead method request then "" else body
              in Route $ responseLBS status ((hContentType, cs contentT) : []) bdy


-- * example use case
--

data FisxUser = FisxUser { name :: String }
  deriving (GHC.Generic)

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
  deriving (GHC.Generic)

instance ToJSON ArianUser

instance (GHC.Generic (WithStatus n a), ToJSON a) => ToJSON (WithStatus n a)

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
main = void . Warp.run 8080 $ serve (Proxy @API) handler


-- * client
--

-- | Copied from "Servant.Client.Core.HasClient".
checkContentTypeHeader :: RunClient m => Response -> m MediaType
checkContentTypeHeader response =
  case lookup "Content-Type" $ toList $ responseHeaders response of
    Nothing -> return $ "application"//"octet-stream"
    Just t -> case parseAccept t of
      Nothing -> throwClientError $ InvalidContentTypeHeader response
      Just t' -> return t'

-- TODO; better name.
proxyOf' :: f a -> Proxy a
proxyOf' _ = Proxy

-- | Given a list of parsers of 'mkres', returns the first one that succeeds and all the
-- failures it encountered along the way
-- TODO; better name, rewrite haddocs.
tryParsers' :: All HasStatus as => Status -> NP (Either String) as -> ([String{- TODO: make this an ADT -}], Maybe (Union as))
tryParsers' _ Nil = (["no statusses match"], Nothing)
tryParsers' status (x :* xs) =
    if status == statusOf x
    then
      case x of
        Left err' ->
          let
            (err'', res) = tryParsers' status xs
          in
            (err' : err'', S <$> res)
        Right res -> ([], Just $ inject (Identity res))
    else -- no reason to parse in the first place. This ain't the one we're looking for
        let
          (err, res) = tryParsers' status xs
        in
          ("Status did not match" : err, S <$> res)

-- | Given a list of types, parses the given response body as each type
mimeUnrenders
  :: forall contentTypes as . All (IsClientResource contentTypes) as
  => LB.ByteString -> NP (Either String) as
mimeUnrenders body = cpure_NP (Proxy @(IsClientResource contentTypes)) (mimeUnrender (Proxy @contentTypes) body)

type IsClientResource contentTypes = MimeUnrender contentTypes `And` HasStatus

-- We are the client, so we're free to pick whatever content type we like!
-- we'll pick the first one
-- If the list of resources is _empty_ we assume the return type is NoContent, and the status code _must_ be 401
-- TODO: Implement that behaviour on the server
instance
  ( RunClient m
  , contentTypes ~ (contentType ': contentTypes')  -- TODO: can we to _ instead of contentTypes'?  probably not.
  , as ~ (a ': as')
  , Accept contentTypes
  , ReflectMethod method
  , All (IsClientResource contentTypes) as
  , All HasStatus as'  -- ?!
  ) => HasClient m (UVerb method contentTypes as) where

  type Client m (UVerb method contentTypes as) = m (Union as)

  clientWithRoute _ _ request = do
    let accept = Seq.fromList . toList . contentTypes $ Proxy @contentTypes
    let method = reflectMethod $ Proxy @method
    response <- runRequest request { requestMethod = method, requestAccept = accept }
    responseContentType <- checkContentTypeHeader response
    unless (any (matches responseContentType) accept) $
      throwClientError $ UnsupportedContentType responseContentType response

    let status = responseStatusCode response
    let body = responseBody response
    let (errors, res) = tryParsers' status $ mimeUnrenders @contentTypes @as body
    case res of
      Nothing -> throwClientError $ DecodeFailure (T.pack (show errors)) response
      Just x -> return x

  hoistClientMonad _ _ nt s = nt s
