-- stack --system-ghc runghc --package servant-docs-simple
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import Servant.API ((:>), Post, ReqBody)
import Servant.Docs.Simple.Parse (parse)
import Servant.Docs.Simple.Render (ApiDocs (..))

-- Our API type
type API = "hello" :> "world" :> Request :> Response
type Request = ReqBody '[()] ()
type Response = Post '[()] ()

-- Intermediate documentation structure
documentTree :: ApiDocs
documentTree = parse @API

-- Raw output of the documentation structure.
-- When actually building documentation,
-- you should pick a suitable Renderable format such as JSON, PlainText etc...
main :: IO ()
main = putStrLn $ show documentTree
