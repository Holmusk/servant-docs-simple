-- stack runghc --package servant-docs-simple
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import Data.Typeable (Typeable, typeRep)

import Servant.API ((:>))
import Servant.Docs.Simple (writeDocsPlainText)
import Servant.Docs.Simple.Parse (HasParsableEndpoint (..), typeText)
import Servant.Docs.Simple.Render (Details (..))

data CustomCombinator a b

-- CustomCombinator documentation
instance (HasParsableEndpoint rest, Typeable a, Typeable b) => HasParsableEndpoint (CustomCombinator a b :> rest) where
    parseEndpoint r a = parseEndpoint @rest r $ a <> [("CustomCombinator"
                                                     , Details [ ("Param A", Detail $ typeText @a)
                                                               , ("Param B", Detail $ typeText @b)
                                                               ]
                                                     )]

--- Just to allow compilation
main :: IO ()
main = putStrLn ""
