-- stack --system-ghc runghc --package servant-docs-simple
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import Data.Text (Text, pack)
import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy (..))

import Servant.API ((:>))
import Servant.Docs.Simple (writeDocsPlainText)
import Servant.Docs.Simple.Parse (HasDocumentApi (..), toDetails, typeText)
import Servant.Docs.Simple.Render (Details (..))

data CustomCombinator a b

-- CustomCombinator documentation
instance (HasDocumentApi rest, Typeable a, Typeable b) => HasDocumentApi (CustomCombinator a b :> rest) where
    document r a = document @rest r $ a <> [("CustomCombinator"
                                            , toDetails [ ("Param A", Detail $ typeText @a)
                                                        , ("Param B", Detail $ typeText @b)
                                                        ]
                                            )]

--- Just to allow compilation
main :: IO ()
main = putStrLn ""
