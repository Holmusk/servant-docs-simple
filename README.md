# servant-docs-simple

[![Build status](https://img.shields.io/travis/holmusk/servant-docs-simple.svg?logo=travis)](https://travis-ci.org/holmusk/servant-docs-simple)
[![Hackage](https://img.shields.io/hackage/v/servant-docs-simple.svg?logo=haskell)](https://hackage.haskell.org/package/servant-docs-simple)
[![Stackage Lts](http://stackage.org/package/servant-docs-simple/badge/lts)](http://stackage.org/lts/package/servant-docs-simple)
[![Stackage Nightly](http://stackage.org/package/servant-docs-simple/badge/nightly)](http://stackage.org/nightly/package/servant-docs-simple)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

This library provides tools to:

1) Parse `Servant API` types into a documentation friendly structure (see Servant.Docs.Simple.Parse)

2) Render the structure into various documentation structures (see Servant.Docs.Simple.Render)

3) Provides convenient functions to write rendered formats to file

# Tutorials

*Generating plaintext/JSON documentation from api types*

``` haskell

{-# LANGUAGE TypeApplications #-}

import Servant.Docs.Simple (documentation)
import Servant.Docs.Simple.Render (Json)
import Data.Aeson (Value)
import Data.Text (Text)

-- Our API type
type API = "hello" :> "world" :> Request :> Response
type Request = ReqBody '[()] ()
type Response = Post '[()] ()

-- Render API as Text documentation
documentText :: Text
documentText = getText $ document @API

-- Render API as Json documentation, represented using Data.Aeson
documentJson :: Value
documentJson = getJson $ documentWith @API @Json

main :: IO ()
main = do
  writeDocsJson "docsJson"           -- Writes to the file $PWD/docsJson
  writeDocsPlainText "docsPlainText" -- Writes to the file $PWD/docsPlainText

```

*Generating the intermediate documentation structure*

``` haskell

{-# LANGUAGE TypeApplications #-}

import Servant.Docs.Simple.Parse (parse)

-- Our API type
type API = "hello" :> "world" :> Request :> Response
type Request = ReqBody '[()] ()
type Response = Post '[()] ()

-- Intermediate documentation structure
documentTree :: Endpoints
documentTree = parse @API

```

*Writing our own rendering format*

``` haskell
-- You may reference Renderable instances in Servant.Docs.Simple.Render for more examples

{-# LANGUAGE TypeApplications #-}

import Servant.Docs.Simple.Parse (parse)
import Servant.Docs.Simple.Render (Renderable (..))

-- Our API type
type API = "hello" :> "world" :> Request :> Response
type Request = ReqBody '[()] ()
type Response = Post '[()] ()

-- Intermediate documentation structure
documentTree :: Endpoints
documentTree = parse @API

-- Our custom datatype which we would like to render to
data Documented = Documented [Endpt] -- A list of documentation for all endpoints

-- Each endpoint consists of
-- Route: (/../../) 
-- Information: (headers, request, response etc..)
data Endpt = Endpt Route Information 
type Route = String
type Information = String

instance Renderable Endpt where
  render (Endpoints ls) = Documented ls'
    where ls' = convert <$> ls
          convert (Node route info) = Endpt route (getInfo info)

-- | Just mush everything together
getInfo :: Details -> String
getInfo (Detail t) = t 
getInfo (Details ls) = foldMap (\(Node t rest) -> t <> getInfo rest) ls

```

# FAQ

- Why use this library when we already have `Servant.Docs`?
