{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Servant.Client.UVerb () where

import Data.ByteString.Lazy (ByteString)
import Data.Foldable (toList)
import Data.Proxy (Proxy(Proxy))
import Data.SOP.BasicFunctors ((:.:)(Comp))
import Data.SOP.Constraint(All, And, Compose)

import Data.SOP.NP (NP(..), cpure_NP)
import Data.SOP.NS (NS(..))
import Servant.API.ContentTypes (MimeUnrender(mimeUnrender), Accept, contentTypes)
import Servant.API (ReflectMethod(reflectMethod))
import Servant.API.UVerb (UVerb, MakesUVerb, HasStatus(..), MakesResource(..), inject)
import Servant.Client.Core (ClientError(..), HasClient(Client, hoistClientMonad, clientWithRoute), RunClient(..), runRequest, requestMethod, responseStatusCode, responseBody, requestAccept)

import Servant.Client.Core.Response
import Network.HTTP.Media (MediaType, parseAccept, (//))

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Network.HTTP.Media                   (matches)
import           Network.HTTP.Types (Status)
import Control.Monad (unless)


-- | Copied from "Servant.Client.Core.HasClient".
checkContentTypeHeader :: RunClient m => Response -> m MediaType
checkContentTypeHeader response =
  case lookup "Content-Type" $ toList $ responseHeaders response of
    Nothing -> return $ "application"//"octet-stream"
    Just t -> case parseAccept t of
      Nothing -> throwClientError $ InvalidContentTypeHeader response
      Just t' -> return t'


proxyOf' :: f a -> Proxy a
proxyOf' _ = Proxy

-- | Given a list of parsers of 'mkres', returns the first one that succeeds and all the
-- failures it encountered along the way
-- TODO: the Nil base-case is never reached because Content types cannot be non-empty in servant
tryParsers' :: All (HasStatus mkres) xs => Status -> NP (Either String :.: mkres) xs -> ([String], Maybe (NS mkres xs))
tryParsers' _ Nil =  (["This should've never happened"], Nothing)
tryParsers' _ (Comp x :* Nil) =
  case x of
    Left err -> ([err], Nothing)
    Right res ->  ([], Just (Z res))
tryParsers' status (Comp x :* xs) =
  -- I am open to suggestions to format this code in a less alien-like way
  let
    status' = getStatus (proxyOf' x)
  in
    if status == status'
    then
      case x of
        Left err' ->
          let
            (err'', res) = tryParsers' status xs
          in
            (err' : err'', S <$> res)
        Right res -> ([], Just $ inject res)
    else -- no reason to parse in the first place. This ain't the one we're looking for
        let
          (err, res) = tryParsers' status xs
        in
          ("Status did not match" : err, S <$> res)


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
-- If the list of resources is _empty_ we assume the return type is NoContent, and the status code _must_ be 401
-- TODO: Implement that behaviour on the server
instance
  ( RunClient m
  , cts ~ ( ct ': cts')
  , resources ~ (res ': resources')  -- make sure the user provides a list of resources
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
      throwClientError $ UnsupportedContentType responseContentType response

    let status = responseStatusCode response
    let body = responseBody response
    let (errors, res) = tryParsers' status  . mimeUnrenders @mkres @ct @resources $ body
    case res of
      Nothing -> throwClientError $ DecodeFailure (T.pack (show errors)) response
      Just x -> return x
  hoistClientMonad Proxy Proxy nt s = nt s
