# servant-docs-simple

[![Build status](https://img.shields.io/travis/holmusk/servant-docs-simple.svg?logo=travis?branch=master)](https://travis-ci.org/holmusk/servant-docs-simple)
[![Hackage](https://img.shields.io/hackage/v/servant-docs-simple.svg?logo=haskell)](https://hackage.haskell.org/package/servant-docs-simple)
[![Stackage Lts](http://stackage.org/package/servant-docs-simple/badge/lts)](http://stackage.org/lts/package/servant-docs-simple)
[![Stackage Nightly](http://stackage.org/package/servant-docs-simple/badge/nightly)](http://stackage.org/nightly/package/servant-docs-simple)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

# Introduction

This library uses [Data.Typeable](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Typeable.html)
to generate documentation for [Servant](https://hackage.haskell.org/package/servant) API types.


It relies on the `typeRep` of Servant's combinators and other datatypes used in
the API to generate the documentation.

**Why do we need this?**


1) We need the `API Format types` inside the documentation. They can be used as keys
   to look up examples, fields, and other miscellaneous details.

2) We want to generate an overview of our endpoints from the information our
   `API` type provides, without having to write instances.

[**In-depth explanation here**](https://github.com/Holmusk/servant-docs-simple#faq)

[**Tutorials**](https://github.com/Holmusk/servant-docs-simple#tutorials)

# Example usage

**Using this script**

``` haskell
-- stack --system-ghc runghc --package servant-docs-simple
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}


module Main where

import Servant.API ((:>), Post, ReqBody)
import Servant.Docs.Simple (writeDocsJson, rriteDocsPlainText)

-- Our API type
type API = "hello" :> "world" :> Request :> Response
type Request = ReqBody '[()] ()
type Response = Post '[()] ()

main :: IO ()
main = do
  -- Writes to the file $PWD/docs.json
  writeDocsJson @API "docs.json"

  -- Writes to the file $PWD/docs.txt
  writeDocsPlainText @API "docs.txt"

```

**Expected Output**

*Files should be generated relative to `$PWD`*

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
            "Format": "[()]",
            "ContentType": "()"
        },
        "RequestType": "'POST",
        "RequestBody": {
            "Format": "[()]",
            "ContentType": "()"
        }
    }
}
```

*docs.txt*

``` text
/hello/world:
RequestBody:
    Format: [()]
    ContentType: ()
RequestType: 'POST
Response:
    Format: [()]
    ContentType: ()
```

# Tutorials

**Click on these links for tutorials**

[Generating plaintext/JSON documentation from api types](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/generate.hs)

[Generating the intermediate documentation structure](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/parse.hs)

[Writing our own rendering format](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/render.hs)

[Parsing custom API combinators](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/custom.hs)

**To run tutorial scripts**

``` sh
# clone this repository
git clone git@github.com:Holmusk/servant-docs-simple.git

# run the examples
stack examples/<source file>
```

# FAQ

**What's the usecase for this?**

1) Lightweight documentation solution by providing an overview of endpoints.

2) Keeping documentation for types in one place

    Suppose we have a library of format types. These are shared between different
    languages and formats. If we centralize all the information in this
    library, we have one source of truth.

    ``` haskell
    --                                           --- Format Types ---
    --                                          |                    |
    --                                          v                    v
    type SingleAPI = "hello" :> ReqBody '[JSON] User :> POST '[JSON] Message
    data User = User Name Password deriving Typeable 
    data Messsage = Message Id Content deriving Typeable

    ```

    We use these format types in our `API` as illustrated above.
    Developers can use these format types to index the library, to find information
    (examples, instances, ...) they need about the type.

**What is required to achieve this?**

1) We need documentation to be generated from the API type, without writing
extra instances for each **format type** we use.

    We can do this with `typeRep` from `Data.Typeable`.

    ``` sh
    $ stack ghci
    Prelude> import Data.Typeable
    Prelude Data.Typeable> data Alignment = Good | Bad deriving Typeable
    Prelude Data.Typeable> typeRep (Proxy :: Proxy Alignment)
    Alignment
    ```

2) We need access to the API format types (`Email`, `Users`, ...) in documentation.

   This is so we can index our library with these types to get the relevant
   fields, examples for these.

**How does our usecase differ from Servant.Docs?**

1) `Servant.Docs` generates documentation only if `Format Types` have
  implemented the necessary instances.

    ``` haskell
    -- Instances for format types; they provide examples for format types

    instance ToSample User where
        toSamples _ = <some example>

    instance ToSample Message where
        toSamples _ = <some example>
    ```

    Hence, we cannot generate documentation solely from the `Format Type`.

2) In documentation generated by `Servant.Docs`, the format types mentioned above (`User`,
`Message`) are not included. This means we can't use them to index our library
to look for the relevant information.

**What are the trade-offs?**

1) Currently `Servant.Docs.Simple` does not support as many formats as
`Servant.Docs` (via `Servant.Docs.Pandoc`).

2) Examples are not mandatory as you do not have to write instances for your format
types.

    This means that **if developers don't provide examples**, the generated
    documentation would be **without them**.

    We can still provide these examples through:

    - The shared library of types

    - Using `Servant combinators` such as `Summary` and `Description`.
      These are generated as part of Documentation through `Servant.Docs.Simple`.
