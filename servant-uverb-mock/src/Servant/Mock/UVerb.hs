{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Mock.UVerb () where

import Control.Monad.IO.Class (liftIO)
import Data.SOP.BasicFunctors ((:.:) (Comp))
import Data.SOP.Constraint (All)
import Data.SOP.NP (cpure_NP)
import Data.SOP.NS (apInjs_NP, sequence'_NS)
import Data.Typeable (Proxy (Proxy))
import Servant.API.UVerb (UVerb, Union)
import Servant.Mock (HasMock (mock))
import Servant.Server (HasServer)
import Servant.Server.UVerb ()
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements, generate)

arbitraryNS :: forall as. All Arbitrary as => Gen (Union as)
arbitraryNS =
  sequence'_NS =<< elements (apInjs_NP (cpure_NP (Proxy @Arbitrary) (Comp arbitrary)))

instance
  ( All Arbitrary as,
    HasServer (UVerb method cts as) ctx
  ) =>
  HasMock (UVerb method cts as) ctx
  where
  mock _ _ = liftIO (generate arbitraryNS)
