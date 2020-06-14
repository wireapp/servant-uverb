{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Servant.Swagger.UVerb () where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict.InsOrd as I
import Data.Proxy (Proxy (Proxy))
import Data.Swagger (ToSchema, Swagger(..), PathItem(..))
import Servant.API.Verbs (Verb)
import Servant.API.UVerb (HasStatus, StatusOf, UVerb, WithStatus(WithStatus))
import Servant.Swagger.Internal (AllAccept, HasSwagger(..), SwaggerMethod)

-- workaround for https://github.com/GetShopTV/swagger2/issues/218
-- We'd like to juse use (<>) but the instances are wrong
combinePathItem :: PathItem -> PathItem -> PathItem
combinePathItem s t = PathItem
  { _pathItemGet = _pathItemGet s <> _pathItemGet t
  , _pathItemPut = _pathItemPut s <> _pathItemPut t
  , _pathItemPost = _pathItemPost s <> _pathItemPost t
  , _pathItemDelete = _pathItemDelete s <> _pathItemDelete t
  , _pathItemOptions = _pathItemOptions s <> _pathItemOptions t
  , _pathItemHead = _pathItemHead s <> _pathItemHead t
  , _pathItemPatch = _pathItemPatch s <> _pathItemPatch t
  , _pathItemParameters = _pathItemParameters s <> _pathItemParameters t
  }

combineSwagger :: Swagger -> Swagger -> Swagger
combineSwagger s t = Swagger
  { _swaggerInfo = _swaggerInfo s <> _swaggerInfo t
  , _swaggerHost = _swaggerHost s <|> _swaggerHost t
  , _swaggerBasePath = _swaggerBasePath s <|> _swaggerBasePath t
  , _swaggerSchemes = _swaggerSchemes s <> _swaggerSchemes t
  , _swaggerConsumes = _swaggerConsumes s <> _swaggerConsumes t
  , _swaggerProduces = _swaggerProduces s <> _swaggerProduces t
  , _swaggerPaths = I.unionWith combinePathItem (_swaggerPaths s) (_swaggerPaths t)
  , _swaggerDefinitions = _swaggerDefinitions s <> _swaggerDefinitions t
  , _swaggerParameters = _swaggerParameters s <> _swaggerParameters t
  , _swaggerResponses = _swaggerResponses s <> _swaggerResponses t
  , _swaggerSecurityDefinitions = _swaggerSecurityDefinitions s <> _swaggerSecurityDefinitions t
  , _swaggerSecurity = _swaggerSecurity s <> _swaggerSecurity t
  , _swaggerTags = _swaggerTags s <> _swaggerTags t
  , _swaggerExternalDocs = _swaggerExternalDocs s <|> _swaggerExternalDocs t
  }

instance HasSwagger (UVerb method cs '[]) where
  toSwagger _ = mempty

instance
  ( ToSchema a,
    HasStatus a,
    AllAccept cs,
    SwaggerMethod method,
    HasSwagger (UVerb method cs as)
  ) =>
  HasSwagger (UVerb method cs (a ': as))
  where
  toSwagger _ =
    toSwagger (Proxy :: Proxy (Verb method (StatusOf a) cs a))
      `combineSwagger` toSwagger (Proxy :: Proxy (UVerb method cs as))

deriving instance ToSchema a => ToSchema (WithStatus s a)
