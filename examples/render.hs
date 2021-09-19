-- stack runghc --package servant-docs-simple
-- You may reference Renderable instances in Servant.Docs.Simple.Render for more examples

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import Data.Map.Ordered (assocs)
import Data.Text (Text)

import Servant.API ((:>), Post, ReqBody)
import Servant.Docs.Simple.Parse (parseApi)
import Servant.Docs.Simple.Render (ApiDocs (..), Details (..), Renderable (..))

-- Our API type
type API = "hello" :> "world" :> Request :> Response
type Request = ReqBody '[()] ()
type Response = Post '[()] ()

-- Intermediate documentation structure
documentTree :: ApiDocs
documentTree = parseApi @API

-- Our custom datatype which we would like to render to
newtype Documented = Documented [Endpt] deriving Show -- A list of documentation for all endpoints

-- Each endpoint consists of
-- Route: (/users/data, /<route name>, etc...)
-- Information: (headers, request, response etc..)
data Endpt = Endpt Route Information deriving Show
type Route = Text
type Information = Text

-- Allow us to render the documentTree in our custom data type
instance Renderable Documented where
  render (ApiDocs endpoints) = Documented endpoints'
    where endpoints' = convert <$> endpoints
          convert (route, info) = Endpt route (getInfo info)

-- Just mush everything together, just a proof of concept.
-- Check out Renderable instances in Servant.Docs.Simple.Render for proper implementations
getInfo :: Details -> Text
getInfo (Detail t)  = t
getInfo (Details ds) = foldMap (\(t, rest) -> t <> getInfo rest) ds

-- Rendered data structure
documented :: Documented
documented = render documentTree

-- Irrelevant, just to allow compile
main :: IO ()
main = print documented
