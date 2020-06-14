{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}

import qualified Network.HTTP.Client as Client
import Control.Concurrent (threadDelay)
import Data.Typeable

-- misc stuff
import Control.Arrow ((+++), left)
import Control.Concurrent.Async
import Control.Monad.Identity

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.String.Conversions
import qualified Data.Text as T

import qualified GHC.Generics as GHC
import GHC.TypeLits

-- sop-core
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.NP (NP(..), cpure_NP)
import Data.SOP.NS

-- network stuff
import Network.HTTP.Media (MediaType, matches, parseAccept, (//))
import Network.HTTP.Types
import Network.Wai (requestHeaders, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp

-- servant
import Servant (ReflectMethod, reflectMethod)
import Servant.API ((:<|>)((:<|>)), (:>), Capture)
import Servant.API.UVerb.OpenUnion
import Servant.API.ContentTypes
import Servant.Client
import Servant.Client.Core (RunClient(..), runRequest, requestMethod, requestAccept)
import Servant.Server
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
  deriving (Eq, Show, GHC.Generic)

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

data ClientParseError = ClientParseError MediaType String | ClientStatusMismatch | ClientNoMatchingStatus
  deriving (Eq, Show, GHC.Generic)

-- | Given a list of parsers of 'mkres', returns the first one that succeeds and all the
-- failures it encountered along the way
-- TODO; better name, rewrite haddocs.
tryParsers' :: All HasStatus as => Status -> NP ([] :.: Either (MediaType, String)) as -> Either [ClientParseError] (Union as)
tryParsers' _ Nil = Left [ClientNoMatchingStatus]
tryParsers' status (Comp x :* xs)
  | status == statusOf (Comp x)
  = case partitionEithers x of
    (err', []) -> (map (uncurry ClientParseError) err' ++) +++ S $ tryParsers' status xs
    (_, (res:_)) -> Right . inject . Identity $ res
  | otherwise -- no reason to parse in the first place. This ain't the one we're looking for
  = (ClientStatusMismatch:) +++ S $ tryParsers' status xs


-- | Given a list of types, parses the given response body as each type
mimeUnrenders
  :: forall contentTypes as . All (AllMimeUnrender contentTypes) as
  => Proxy contentTypes -> LB.ByteString -> NP ([] :.: Either (MediaType, String)) as
mimeUnrenders ctp body = cpure_NP (Proxy @(AllMimeUnrender contentTypes)) (Comp . map (\(mediaType, parser) -> left ((,) mediaType) (parser body)) . allMimeUnrender $ ctp)

instance
  ( RunClient m
  , contentTypes ~ (contentType ': contentTypes')  -- TODO: can we to _ instead of contentTypes'?  probably not.
  , as ~ (a ': as')
  , AllMime contentTypes
  , ReflectMethod method
  , All (AllMimeUnrender contentTypes) as
  , All HasStatus as
  ) => HasClient m (UVerb method contentTypes as) where

  type Client m (UVerb method contentTypes as) = m (Union as)

  clientWithRoute _ _ request = do
    let accept = Seq.fromList . allMime $ Proxy @contentTypes
        -- TODO(fisx): we want to send an accept header with, say, the first content type
        -- supported by the api, so we don't have to parse all of them, no?  not sure i'm
        -- missing anything here.

        method = reflectMethod $ Proxy @method
    response <- runRequest request { requestMethod = method, requestAccept = accept }
    responseContentType <- checkContentTypeHeader response
    unless (any (matches responseContentType) accept) $
      throwClientError $ UnsupportedContentType responseContentType response

    let status = responseStatusCode response
        body = responseBody response
        res = tryParsers' status $ mimeUnrenders (Proxy @contentTypes) body
    case res of
      Left errors -> throwClientError $ DecodeFailure (T.pack (show errors)) response
      Right x -> return x

  hoistClientMonad _ _ nt s = nt s

-- | convenience function to extract an unknown union element using a type class.
collapseUResp :: forall c a as. All c as
  => Proxy (c :: * -> Constraint) -> (forall x. c x => x -> a) -> Union as -> a
collapseUResp proxy render = collapse_NS . cmap_NS proxy (K . render . runIdentity)

-- | convenience function to extract an unknown union element using 'cast'.
extractUResp :: forall a as. (All Typeable as, Typeable a) => Union as -> Maybe a
extractUResp = collapse_NS . cmap_NS (Proxy @Typeable) (K . cast . runIdentity)


-- * example use case
--

data FisxUser = FisxUser { name :: String }
  deriving (Eq, Show, GHC.Generic)

instance ToJSON FisxUser
instance FromJSON FisxUser

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
  deriving (Eq, Show, GHC.Generic)

instance ToJSON ArianUser
instance FromJSON ArianUser

instance (GHC.Generic (WithStatus n a), ToJSON a) => ToJSON (WithStatus n a)
instance (GHC.Generic (WithStatus n a), FromJSON a) => FromJSON (WithStatus n a)

type API = "fisx" :> Capture "bool" Bool :> UVerb 'GET '[JSON] '[FisxUser, WithStatus 303 String]
      :<|> "arian" :> UVerb 'GET '[JSON] '[WithStatus 201 ArianUser]

fisx :: Bool -> Handler (Union '[FisxUser, WithStatus 303 String])
fisx True = respond (FisxUser "fisx")
fisx False = respond (WithStatus @303 ("still fisx" :: String))

arian :: Handler (Union '[WithStatus 201 ArianUser])
arian = respond (WithStatus @201 ArianUser)

handler :: Server API
handler = fisx :<|> arian


fisxClient :: Bool -> ClientM (Union '[FisxUser, WithStatus 303 String])
arianClient :: ClientM (Union '[WithStatus 201 ArianUser])
(fisxClient :<|> arianClient) = client (Proxy @API)


main :: IO ()
main = do
  _ <- async . Warp.run 8080 $ serve (Proxy @API) handler
  threadDelay 50000
  mgr <- Client.newManager Client.defaultManagerSettings
  let cenv = mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")
  result <- runClientM (fisxClient True) cenv
  print $ collapseUResp (Proxy @Show) show <$> result
  print $ extractUResp @FisxUser <$> result
  print $ extractUResp @(WithStatus 303 String) <$> result

  pure ()
