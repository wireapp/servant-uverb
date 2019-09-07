{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Mock.UVerb
(
)
where
import Control.Monad.IO.Class (liftIO)
import Servant.API.UVerb
import Servant.Server (HasServer)
import Servant.Mock

import Data.Proxy (Proxy(Proxy))
import Data.SOP.Constraint(All, Compose)
import Data.SOP.BasicFunctors ((:.:)(Comp))
import Data.SOP.NS (NS(..), apInjs_NP, sequence'_NS)
import Data.SOP.NP (cpure_NP) 

import Test.QuickCheck (Gen, Arbitrary(arbitrary), generate, elements)
{-instance (Arbitrary a, KnownNat status, ReflectMethod method, AllCTRender ctypes a)
    => HasGenerate (Verb method status ctypes a) context where
  mock _ _ = mockArbitrary -}



arbitraryNS :: forall f xs. All (Arbitrary `Compose` f) xs =>  Gen (NS f xs)
arbitraryNS =
  sequence'_NS =<< elements (apInjs_NP (cpure_NP (Proxy @(Arbitrary `Compose` f)) (Comp arbitrary)))



instance 
  ( All (Compose Arbitrary mkres) resources
  , HasServer (UVerb mkres method cts resources) ctx 
  ) => HasMock (UVerb mkres method cts resources) ctx where
  mock _ _ = liftIO (generate arbitraryNS)



