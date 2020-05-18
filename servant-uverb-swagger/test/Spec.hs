{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Monad (void)
import Data.Aeson
import Data.Proxy
import Data.SOP.NS
import Data.Swagger.Schema
import GHC.Generics
import Network.HTTP.Types
import Servant.API
import Servant.API.UVerb
import Servant.Server
import Servant.Server.UVerb ()
import Servant.Swagger
import System.Timeout (timeout)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Swagger.UI as UI


main :: IO ()
main = void $ timeout 1_000_000 run


-- | Application-specific resource marker.  By wrapping application types defined in other
-- modules with this, we can write non-orphan 'HasStatus' instances.
newtype Resource (value :: *) = Resource (value :: *)
  deriving newtype (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

instance MakesResource Resource value where
  mkResource = Resource

type API
     = "a" :> Capture "flag" Bool :> UVerb Resource 'GET '[JSON] '[Bool]
  :<|> "b" :> Capture "flag" Bool :> UVerb Resource 'GET '[JSON] '[Bool, String]

instance HasStatus Resource Bool   where getStatus _ = status201
instance HasStatus Resource String where getStatus _ = status303

respond :: a
respond = undefined

aHandler :: Bool -> Handler (NS Resource '[Bool])
aHandler = respond

bHandler :: Bool -> Handler (NS Resource '[Bool, String])
bHandler True  = respond ("Wrong" :: String)
bHandler False = respond True

api :: Server API
api  = aHandler
  :<|> bHandler

type API' = UI.SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> API

api' :: Server API'
api' = UI.swaggerSchemaUIServer (toSwagger (Proxy @API)) :<|> api

app :: Application
app = serve (Proxy @API') api'

run :: IO ()
run = Warp.run 8080 app


instance HasSwagger (UVerb
                      (mkres :: * -> *)
                      (method :: StdMethod)
                      (ctx :: [*])
                      (resources :: [*])) where
  toSwagger = undefined

{-

instance  {-# OVERLAPPABLE #-}
  forall mkres method cts xs.
  ( SwaggerMethod method
  , AllAccept cts
  , All ToSchema xs
  , All (HasStatus mkres) xs
  ) => HasSwagger (UVerb mkres method cts xs) where
  toSwagger Proxy = undefined

f :: forall (mkres :: * -> *) (x :: *) a. (HasStatus mkres x, ToSchema (mkres x)) => Proxy mkres -> Proxy x -> a
f _ _ = at status ?~ _ -- at status ?~ Inline (mempty & schema ?~ res)
--         )) & definitions .~ defs
    where
      status = statusCode $ getStatus (Proxy @(mkres x))
      (defs, ref) = runDeclare (declareSchemaRef (Proxy @(mkres x))) mempty

-- f needs to be in some swagger monad declareSchemaRef yields the ref, and then allfs will
-- iterate f over all xs and collect a list of result options, and *then* the definitions will
-- be extracted using runDeclare, probably in the @instance HasSwagger UVerb@?

-}
