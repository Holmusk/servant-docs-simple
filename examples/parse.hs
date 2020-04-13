-- stack --system-ghc runghc --package servant-docs-simple
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import Servant.API ((:>), Post, ReqBody)
import Servant.Docs.Simple.Parse (parse)
import Servant.Docs.Simple.Render (Endpoints (..))

-- Our API type
type API = "hello" :> "world" :> Request :> Response
type Request = ReqBody '[()] ()
type Response = Post '[()] ()

-- Intermediate documentation structure
documentTree :: Endpoints
documentTree = parse @API

-- Irrelevant, just to allow compile
main :: IO ()
main = putStrLn "hello world"
