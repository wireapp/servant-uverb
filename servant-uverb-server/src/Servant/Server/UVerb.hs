module Servant.Server.UVerb where

import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.NS
import Data.String.Conversions
import Network.HTTP.Types (Status, hContentType, hAccept)
import Network.Wai (requestHeaders, responseLBS)
import Servant.API.ContentTypes
import Servant.API.ContentTypes (AcceptHeader (..), AllCTRender (..))
import Servant (ReflectMethod, reflectMethod)
import Servant.Server.Internal
import Servant.API.UVerb


-- | Helper constraint used in @instance 'HasServer' 'UVerb'@.
type IsResource cts mkres =
  Compose (AllCTRender cts) mkres `And`
    HasStatus mkres `And`
    MakesResource mkres

instance
  ( ReflectMethod method
  , AllMime cts
  , All (IsResource cts mkres) resources
  , MakesUVerb mkres method cts resources
  ) => HasServer (UVerb mkres method cts resources) context where

  type ServerT (UVerb mkres method cts resources) m = m (NS mkres resources)

  hoistServerWithContext _ _ nt s = nt s

  route Proxy _ctx action = leafRouter route'
    where
      method = reflectMethod (Proxy @method)

      route' env request cont = do
        let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
            action' = action
                `addMethodCheck` methodCheck method request
                `addAcceptCheck` acceptCheck (Proxy @cts) accH

            mkProxy :: a -> Proxy a
            mkProxy _ = Proxy

        runAction action' env request cont $ \(output :: NS mkres resources) -> do
          let encodeResource
                :: forall resource.
                   IsResource cts mkres resource =>
                   mkres resource -> K (Status, Maybe (LBS, LBS)) resource
              encodeResource res = K
                  ( getStatus $ mkProxy res
                  , handleAcceptH (Proxy @cts) (AcceptHeader accH) res
                  )

              pickResource
                :: NS mkres resources -> (Status, Maybe (LBS, LBS))
              pickResource = collapse_NS . cmap_NS (Proxy @(IsResource cts mkres)) encodeResource

          case pickResource output of
            (_, Nothing) -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
            (status, Just (contentT, body)) ->
              let bdy = if allowedMethodHead method request then "" else body
              in Route $ responseLBS status ((hContentType, cs contentT) : []) bdy

-- | 'return' for 'UVerb' handlers.  Pass it a value of an application type from the routing
-- table, and it will return a value of the union of responses.
respond
  :: forall (f :: * -> *) (mkres :: * -> *) (x :: *) (xs :: [*]).
     (Applicative f, MakesResource mkres x, IsMember x xs)
  => x -> f (NS mkres xs)
respond = pure . inject . mkResource
