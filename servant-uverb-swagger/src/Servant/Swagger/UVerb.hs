{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Servant.Swagger.UVerb () where

import Data.Proxy (Proxy (Proxy))
import Data.Swagger (ToSchema)
import Servant.API.Verbs (Verb)
import Servant.API.UVerb (HasStatus, StatusOf, UVerb, WithStatus)
import Servant.Swagger.Internal (AllAccept, HasSwagger(..), SwaggerMethod)

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
      <> toSwagger (Proxy :: Proxy (UVerb method cs as))

instance ToSchema a => ToSchema (WithStatus s a) where
