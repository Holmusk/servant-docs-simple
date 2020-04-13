-- stack --system-ghc runghc --package servant-docs-simple
-- You may reference Renderable instances in Servant.Docs.Simple.Render for more examples

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import Data.Text (Text)

import Servant.API ((:>), Post, ReqBody)
import Servant.Docs.Simple.Parse (parse)
import Servant.Docs.Simple.Render (Details (..), Endpoints (..), Node (..), Renderable (..))

-- Our API type
type API = "hello" :> "world" :> Request :> Response
type Request = ReqBody '[()] ()
type Response = Post '[()] ()

-- Intermediate documentation structure
documentTree :: Endpoints
documentTree = parse @API

-- Our custom datatype which we would like to render to
newtype Documented = Documented [Endpt] -- A list of documentation for all endpoints

-- Each endpoint consists of
-- Route: (/users/data, /<route name>, etc...)
-- Information: (headers, request, response etc..)
data Endpt = Endpt Route Information
type Route = Text
type Information = Text

-- Allow us to render the documentTree in our custom data type
instance Renderable Documented where
  render (Endpoints ls) = Documented ls'
    where ls' = convert <$> ls
          convert (Node route info) = Endpt route (getInfo info)

-- Just mush everything together
getInfo :: Details -> Text
getInfo (Detail t)   = t
getInfo (Details ls) = foldMap (\(Node t rest) -> t <> getInfo rest) ls

-- Irrelevant, just to allow compile
main :: IO ()
main = putStrLn "hello world"
