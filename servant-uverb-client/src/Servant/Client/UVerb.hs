{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Servant.Client.UVerb () where

import Data.ByteString.Lazy (ByteString)
import Data.Foldable (toList)
import Data.Proxy (Proxy(Proxy))
import Data.SOP.BasicFunctors (I(I),(:.:)(Comp))
import Data.SOP.Constraint(All, And, Compose)

import GHC.TypeLits
import Data.SOP.NP (NP(..), cpure_NP)
import Data.SOP.NS (NS(..))
import Servant.API.ContentTypes (MimeUnrender(mimeUnrender), Accept, contentTypes)
import Servant.API (ReflectMethod(reflectMethod))
import Servant.API.UVerb (UVerb, MakesUVerb, HasStatus(..), MakesResource(..), inject)
import Servant.Client.Core (ServantError(..),HasClient(Client, hoistClientMonad, clientWithRoute), RunClient(..), runRequest, requestMethod, responseStatusCode, responseBody, requestAccept)

import Servant.Client.Core.Internal.RunClient (checkContentTypeHeader)

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Network.HTTP.Media                   (matches)
import Control.Monad (unless)

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

proxyOf' :: f a -> Proxy a
proxyOf' _ = Proxy

-- | Given a list of parsers of 'mkres', returns the first one that succeeds and all the
-- failures it encountered along the way
tryParsers' :: All (HasStatus mkres) xs => err -> NP (Either err :.: mkres) xs -> ([err], Maybe (NS mkres xs))
tryParsers' err Nil =  ([err], Nothing)
tryParsers' _ (Comp x :* Nil) =
  case x of
    Left err -> ([err], Nothing)
    Right res ->  ([], Just (Z res))
tryParsers' _ (Comp x :* xs) =
  -- Let us check what the associated http status is for this resource.  If it
  -- doesn't match the one that the server responded with, we know we can skip
  -- this parse
  let status' = getStatus (proxyOf' x)
  in
  -- force the computation of the parser only if the status matches (optimization)
  case x of
    Left err' -> 
      let (err'', res) = tryParsers' err' xs
      in (err' : err'', S <$> res)
    Right res -> ([], Just $ inject res)


type IsResource ct mkres =
    (MimeUnrender ct `Compose` mkres) `And`
    HasStatus mkres `And`
    MakesResource mkres

-- | Given a list of types, parses the given response body as each type
mimeUnrenders 
  :: forall mkres ct xs . All (IsResource ct mkres) xs 
  => ByteString -> NP (Either String :.: mkres) xs
mimeUnrenders body = cpure_NP (Proxy @(IsResource ct mkres)) (Comp $ mimeUnrender (Proxy @ct) body)

-- We are the client, so we're free to pick whatever content type we like!
-- we'll pick the first one
instance 
  ( RunClient m 
  , cts ~ ( ct ': cts')
  , Accept ct
  , ReflectMethod method
  , All (IsResource ct mkres) resources
  , MakesUVerb mkres method cts resources
  ) => HasClient m (UVerb  mkres method cts resources) where

  type Client m (UVerb mkres method cts resources) = m (NS mkres resources)

  clientWithRoute Proxy Proxy request = do
    let accept = Seq.fromList . toList . contentTypes $ Proxy @ct
    let method = reflectMethod $ Proxy @method
    response <- runRequest request { requestMethod = method, requestAccept = accept }
    responseContentType <- checkContentTypeHeader response
    unless (any (matches responseContentType) accept) $
      throwServantError $ UnsupportedContentType responseContentType response

    let status = responseStatusCode response
    let body = responseBody response
    let (errors, res) = tryParsers' "no content types (shouldn't happen?)" . mimeUnrenders @mkres @ct @resources $ body
    case res of
      Nothing -> throwServantError $ DecodeFailure (T.pack (show errors)) response
      Just x -> return x
  hoistClientMonad Proxy Proxy nt s = nt s


