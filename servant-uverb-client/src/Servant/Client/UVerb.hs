module Servant.Client.UVerb () where

import Data.Proxy
import Data.SOP.NS
import Data.SOP.NP
import Data.SOP.Constraint
import Data.SOP.BasicFunctors
import Servant.Client.Core

import Servant.API.UVerb

class CanParse a where
  -- TODO this must become :: Request -> Either String a, but lets have some imagination and
  -- think Request is pre-applied already
  parser :: Either String a
  


-- given a list of types that are parseable, give a list of parsers. one for each type
makeParsers :: All CanParse xs => Proxy xs -> NP (Either String) xs
makeParsers Proxy = cpure_NP (Proxy @CanParse) parser

-- now we have a list of parsers
parsers :: NP (Either String) '[Int, String, Char]
parsers = Left "also no" :* Left "no" :* Right '3' :* Nil

-- turn a list of parsers into a list of sums of parsers
test :: [NS (Either String) '[Int, String, Char]]
test = apInjs_NP parsers

--- pick the first thing that parses
test2 :: Either String (NS I '[Int, String, Char])
test2 = pickFirstParse test

-- FUTUREWORK: something along this lines probably already exists for some good choice of f, f', g
-- e.g. traverse'_NS :: forall xs f f' g. (SListI xs, Functor g) => (forall a. f a -> g (f' a)) -> NS f xs -> g (NS f' xs) 
-- has a very similar type
--
-- Fun one for @fisx
transformIfItSucceeded :: NS (Either String) xs -> Either String (NS I xs)
transformIfItSucceeded (Z (Left x)) = Left x
transformIfItSucceeded (Z (Right y)) = Right (Z (I y))
transformIfItSucceeded (S y) = S <$> transformIfItSucceeded y



-- FUTUREWORK: Use  The Validation semigroup here so we can collect all the error messages
pickFirstParse :: [(NS (Either String)) xs] -> Either String (NS I xs)
pickFirstParse [] = Left "none of them parsed"
pickFirstParse (x : xs) =
    case transformIfItSucceeded x of
      Left x -> pickFirstParse xs
      Right y -> Right y
  

instance RunClient m => HasClient m (UVerb  mkres method cts resources) where
  type Client m (UVerb mkres method cts resources) = m (NS mkres resources)
  clientWithRoute Proxy Proxy req = undefined
  hoistClientMonad Proxy Proxy f c = f c
