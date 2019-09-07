{-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

import Servant
import Servant.Server.Generic
import Servant.API.Generic
import Servant.API.UVerb
import Servant.Mock.UVerb
import Data.Aeson.Types
import Data.SOP.NS
import Data.SOP.BasicFunctors (I(..))
import Test.QuickCheck (Arbitrary)
import Servant.Mock
import GHC.TypeLits
import Network.Wai.Handler.Warp

data NotFound = NotFound { msg :: String }
  deriving stock (Generic)
  deriving anyclass (ToJSON, Arbitrary)

data UserUnauthorized = UserUnauthorized { msg :: String }
  deriving stock (Generic)
  deriving anyclass (ToJSON,  Arbitrary)

data UserView = UserView { name :: String }
  deriving stock (Generic)
  deriving anyclass (ToJSON, Arbitrary)

data CreateUser = CreateUser { name :: String}
  deriving stock (Generic)
  deriving anyclass (FromJSON, Arbitrary)

data UserCreated = UserCreated { name :: String }
  deriving stock (Generic)
  deriving anyclass (ToJSON, Arbitrary)


newtype WithStatus n a = WithStatus a
  deriving newtype (FromJSON, ToJSON, Arbitrary)
  deriving stock (Functor)

-- TODO findbetter place
instance KnownNat n => HasStatus I (WithStatus n a) where
  getStatus _ = toEnum $ fromInteger $ natVal (Proxy @n)

instance MakesResource I (WithStatus n a) where
  mkResource = I

deriving newtype instance ToJSON a => ToJSON (I a)
deriving newtype instance Arbitrary a => Arbitrary (I a)

data Routes route = Routes
  { getUser :: route :- Description "gets a user" 
                 :> Capture' '[Description "The id to use"] "id" Int
                 :> UVerb I 'GET '[JSON] '[ WithStatus 200 UserView , WithStatus 404 NotFound ]
  , putUser :: route :- Description "Creates a user" 
                 :> ReqBody' '[Description "The user you want to create"] '[JSON] CreateUser
                 :> UVerb I 'PUT '[JSON] '[ WithStatus 201 UserCreated, WithStatus 401 UserUnauthorized ]
  }
  deriving (Generic)


api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy @Routes)

handler :: Server (ToServantApi Routes)
handler = const (respond (WithStatus @404 (NotFound "nop"))) :<|> const (respond (WithStatus @401 (UserUnauthorized "no")))
-- uncomment this and you get "empty reply"
--handler = mock api (Proxy @('[]))

app :: Application
-- app = serve api (mock api (Proxy @('[])))
app = serve api handler

main :: IO ()
main = run 8080 app
