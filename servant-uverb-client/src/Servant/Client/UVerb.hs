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
import Servant.API.UVerb (UVerb, MakesUVerb, HasStatus, MakesResource, inject)
import Servant.Client.Core (HasClient(Client, hoistClientMonad, clientWithRoute), RunClient, runRequest, requestMethod, responseStatusCode, responseBody, requestAccept)

import qualified Data.Sequence as Seq

-- TODO this is some kind of WriterT I suppose?.
-- Ask Andres to Code Golf the shit out of this
-- | Given a list of parsers of 'mkres', returns the first one that succeeds and all the
-- failures it encountered along the way
tryParsers' :: err -> NP (Either err :.: mkres) xs -> ([err], Maybe (NS mkres xs))
tryParsers' err Nil =  ([err], Nothing)
tryParsers' _ (x :* Nil) =
  case x of
    Comp (Left err) -> ([err], Nothing)
    Comp (Right res) -> ([], Just (Z res))
tryParsers' _ (x :* xs) =
  case x of
    Comp (Left err') -> 
      let (err'', res) = tryParsers' err' xs
      in (err' : err'', S <$> res)
    Comp (Right res) -> ([], Just $ inject res)

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
    let _status = responseStatusCode response
    let body = responseBody response
    let (err, res) = tryParsers' "???" . mimeUnrenders @mkres @ct @resources $ body
    case res of
      Nothing -> error (show err)
      Just x -> return x
  hoistClientMonad Proxy Proxy nt s = nt s


