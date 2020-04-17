# servant-docs-simple

[![Build status](https://img.shields.io/travis/holmusk/servant-docs-simple.svg?logo=travis)](https://travis-ci.org/holmusk/servant-docs-simple)
[![Hackage](https://img.shields.io/hackage/v/servant-docs-simple.svg?logo=haskell)](https://hackage.haskell.org/package/servant-docs-simple)
[![Stackage Lts](http://stackage.org/package/servant-docs-simple/badge/lts)](http://stackage.org/lts/package/servant-docs-simple)
[![Stackage Nightly](http://stackage.org/package/servant-docs-simple/badge/nightly)](http://stackage.org/nightly/package/servant-docs-simple)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

# Introduction

This library uses [Data.Typeable](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Typeable.html)
to generate documentation for [Servant](https://hackage.haskell.org/package/servant) API types.

It relies on the `typeRep` of Servant's combinators and other datatypes used in
the API to generate the documentation.

[What's the usecase for this?](https://github.com/Holmusk/servant-docs-simple#faq)

# Functionality

## Servant.Docs.Simple.Parse

*Parses the API into a documentation friendly structure*

**API type**

``` haskell
type API = "hello" :> "world" :> Request :> Response
type Request = ReqBody '[()] ()
type Response = Post '[()] ()

```

**Intermediate structure**

``` haskell
Endpoints [Node "/hello/world" 
                (Details [ Node "RequestBody" (Details [ Node "Format" 
                                                              (Detail "': * () ('[] *)")
                                                       , Node "ContentType"
                                                              (Detail "()")
                                                       ])
                         , Node "RequestType" (Detail "'POST")
                         , Node "Response" (Details [ Node "Format"
                                                           (Detail "': * () ('[] *)")
                                                    , Node "ContentType"
                                                           (Detail "()")
                                                    ])
                         ])]
```

## Servant.Docs.Simple.Render

*Renders the intermediate structure into common documentation formats*

**Intermediate structure**

``` haskell
Endpoints [Node "/hello/world" 
                (Details [ Node "RequestBody" (Details [ Node "Format" 
                                                              (Detail "': * () ('[] *)")
                                                       , Node "ContentType"
                                                              (Detail "()")
                                                       ])
                         , Node "RequestType" (Detail "'POST")
                         , Node "Response" (Details [ Node "Format"
                                                           (Detail "': * () ('[] *)")
                                                    , Node "ContentType"
                                                           (Detail "()")
                                                    ])
                         ])]
```


**JSON**

``` json
{
    "/hello/world": {
        "Response": {
            "Format": "': * () ('[] *)",
            "ContentType": "()"
        },
        "RequestType": "'POST",
        "RequestBody": {
            "Format": "': * () ('[] *)",
            "ContentType": "()"
        }
    }
}
```

**Text**

``` text
/hello/world:
RequestBody:
    Format: ': * () ('[] *)
    ContentType: ()
RequestType: 'POST
Response:
    Format: ': * () ('[] *)
    ContentType: ()
```

## Servant.Docs.Simple

*Provides functions to write rendered formats to file/stdout*

**Using this script**

``` haskell
-- stack --system-ghc runghc --package servant-docs-simple
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}


module Main where

import Data.Aeson (Value)
import Data.Text (Text)

import Servant.API ((:>), Post, ReqBody)
import Servant.Docs.Simple (document, documentWith, stdoutJson, stdoutPlainText, writeDocsJson,
                            writeDocsPlainText)
import Servant.Docs.Simple.Render (Json (..), PlainText (..))

-- Our API type
type API = "hello" :> "world" :> Request :> Response
type Request = ReqBody '[()] ()
type Response = Post '[()] ()

main :: IO ()
main = do
  -- Writes to the file $PWD/docsJson
  writeDocsJson @API "docs.json"

  -- Writes to the file $PWD/docsPlainText
  writeDocsPlainText @API "docs.txt" 
```

**Expected Output**

*Files should be generated relative to $PWD*

``` sh
$ ls | grep docs
docs.json
docs.txt
```

*docs.json*

``` json
{
    "/hello/world": {
        "Response": {
            "Format": "': * () ('[] *)",
            "ContentType": "()"
        },
        "RequestType": "'POST",
        "RequestBody": {
            "Format": "': * () ('[] *)",
            "ContentType": "()"
        }
    }
}
```

*docs.txt*

``` text
/hello/world:
RequestBody:
    Format: ': * () ('[] *)
    ContentType: ()
RequestType: 'POST
Response:
    Format: ': * () ('[] *)
    ContentType: ()
```

# Tutorials

**Click on these links for tutorials**

[Generating plaintext/JSON documentation from api types](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/render.hs)

[Generating the intermediate documentation structure](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/parse.hs)

[Writing our own rendering format](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/format.hs)

**To run tutorial scripts**

``` sh
# clone this repository
git clone git@github.com:Holmusk/servant-docs-simple.git

# run the examples
stack examples/<source file>
```

# FAQ

## What's the usecase for this?

**TLDR**

1) We need the typeRep of `API Format types` to be inside the documentation.

2) We want to generate documentation from the information our `API` type provides, without having to write instances.

**In depth explanation**

`Servant.Docs` generates documentation after instances are written for `API
Format Types`.

``` haskell
--                                           --- Format Types ---
--                                          |                    |
--                                          v                    v
type singleAPI = "hello" :> ReqBody '[JSON] User :> POST '[JSON] Message
type User = ...
type Messsage = ...

```

These instances provide human-friendly examples for what your
`RequestBody` to the endpoint should contain, what you can expect from the
endpoint's `Response`, and so on.

``` haskell
instance ToSample User where
    toSamples _ = <some example>

instance ToSample Message where
    toSamples _ = <some example>
```

In the actual documentation generated by `Servant.Docs`, the format types mentioned above (`User`,
`Message`) are not mentioned.

We need access to the `typeRep` of `API format types` (`Email`, `Users`, ...) because we have a shared
library of types. As such developers can use the API format types to index the
library finding relevant information they need about these format types.

This functionality can be provided with `Data.Typeable`.

``` sh
$ stack ghci
Prelude> import Data.Typeable
Prelude Data.Typeable> data Alignment = Good | Bad deriving Typeable
Prelude Data.Typeable> typeRep (Proxy :: Proxy Alignment)
Alignment
```

The trade-off made here is that human-friendly examples are not mandatory. It is
now dependent on:

1) The shared library of types to provide such information or

OR

2) Using in built combinators such as `Summary` and `Description` to generate these.
