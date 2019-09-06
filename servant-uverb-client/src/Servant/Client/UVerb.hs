{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Servant.Client.UVerb () where

import Data.ByteString.Lazy (ByteString)
import Data.Foldable (toList)
import Data.Proxy (Proxy(Proxy))
import Data.SOP.BasicFunctors ((:.:)(Comp))
import Data.SOP.Constraint(All, And, Compose)

import Data.SOP.NP (NP, cpure_NP)
import Data.SOP.NS (NS, apInjs_NP, sequence'_NS)
import Servant.API.ContentTypes (MimeUnrender(mimeUnrender), Accept, contentTypes)
import Servant.API (ReflectMethod(reflectMethod))
import Servant.API.UVerb (UVerb, MakesUVerb, HasStatus, MakesResource)
import Servant.Client.Core (HasClient(Client, hoistClientMonad, clientWithRoute), RunClient, runRequest, requestMethod, responseStatusCode, responseBody, requestAccept)

import qualified Data.Sequence as Seq
  

-- TODO Collect all error messages, not just the last one
pickFirstParse :: m -> [(NS (Either m :.: mkres)) xs] -> Either m (NS mkres xs)
pickFirstParse m [] = Left m
pickFirstParse m (x : xs) =
  case sequence'_NS x of
    Left _ -> pickFirstParse m xs
    Right y -> Right y
  where

-- collapse_NP  :: NP (K a) xs     -> [a]
-- collapse_NS  :: NS (K a) xs     -> a
-- apInjs'_NP   :: NP f xs         -> NP (K (NS f xs)) xs
-- sequence'_NP :: NP (Eiher m :.: mkres) xs -> Either m (NP mkres xs)
-- sequence'_NS :: NS (f :.: g) xs -> f (NS g xs)

-- | Helper constraint used in @instance 'Client' 'UVerb'@.
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
    let parsersOf = apInjs_NP . mimeUnrenders @mkres @ct @resources
    case pickFirstParse "none" . parsersOf $ body of
      Left x -> error x -- TODO we need to do better here. See servant-client-core source code :) But we're close!
      Right x -> return x

  hoistClientMonad Proxy Proxy nt s = nt s


