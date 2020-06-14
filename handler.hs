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
{-# OPTIONS_GHC -Wall -Wno-orphans #-}

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Data.Aeson
import Data.Typeable
import qualified GHC.Generics as GHC
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as Warp
import Servant.API
import Servant.API.UVerb
import Servant.Client
import Servant.Client.UVerb
import Servant.Server
import Servant.Server.UVerb

data FisxUser = FisxUser {name :: String}
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

type API =
  "fisx" :> Capture "bool" Bool :> UVerb 'GET '[JSON] '[FisxUser, WithStatus 303 String]
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
  -- print $ toSwagger (Proxy @API)
  pure ()

-- TODO: UStream (like 'Stream')
-- TODO: NoContentUVerb (like 'NoContentVerb')
