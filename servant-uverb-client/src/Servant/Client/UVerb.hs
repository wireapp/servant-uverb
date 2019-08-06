module Servant.Client.UVerb () where

import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.SOP.NP
import Data.SOP.NS
import qualified Data.Sequence as Seq
import Data.SOP.Constraint
import Data.SOP.Classes (HSequence(hsequence'))
import Data.SOP.BasicFunctors
import Servant.Client.Core
import Servant.API.ContentTypes
import Servant.API (ReflectMethod(reflectMethod))
import Data.Foldable (toList)

import Servant.API.UVerb


transformIfItSucceeded :: NS (Either String :.: mkres) xs -> Either String (NS mkres xs)
transformIfItSucceeded = sequence'_NS



-- FUTUREWORK: Use  The Validation semigroup here so we can collect all the error messages
pickFirstParse :: [(NS (Either String :.: mkres)) xs] -> Either String (NS mkres xs)
pickFirstParse [] = Left "none of them parsed"
pickFirstParse (x : xs) =
    case transformIfItSucceeded x of
      Left x -> pickFirstParse xs
      Right y -> Right y
  
-- | Helper constraint used in @instance 'Client' 'UVerb'@.
type IsResource ct mkres =
  ( Compose (MimeUnrender ct) mkres `And`
    HasStatus mkres `And`
    MakesResource mkres
  )

-- | Given a list of types, parses the given response body as each type
--
-- TODO should return an NP (Either String 
mimeUnrenders 
  :: forall ct xs mkres. All (IsResource ct mkres) xs 
  => Proxy mkres -> Proxy ct -> Proxy xs -> ByteString -> NP (Either String :.: mkres) xs
mimeUnrenders mkres ct xs body = cpure_NP (Proxy @(IsResource ct mkres)) (Comp $ mimeUnrender ct body)

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
    let status = responseStatusCode response
    let body = responseBody response
    let resp = pickFirstParse . apInjs_NP . mimeUnrenders (Proxy @mkres) (Proxy @ct) (Proxy @resources) $ body
    case resp of
      Left x -> error x -- TODO we need to do better here. See servant-client-core source code :) But we're close!
      Right x -> return x

  hoistClientMonad Proxy Proxy nt s = nt s
