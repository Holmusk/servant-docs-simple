# servant-docs-simple

[![Build status](https://img.shields.io/travis/holmusk/servant-docs-simple.svg?logo=travis)](https://travis-ci.org/holmusk/servant-docs-simple)
[![Hackage](https://img.shields.io/hackage/v/servant-docs-simple.svg?logo=haskell)](https://hackage.haskell.org/package/servant-docs-simple)
[![Stackage Lts](http://stackage.org/package/servant-docs-simple/badge/lts)](http://stackage.org/lts/package/servant-docs-simple)
[![Stackage Nightly](http://stackage.org/package/servant-docs-simple/badge/nightly)](http://stackage.org/nightly/package/servant-docs-simple)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

# Introduction

This library uses
[Data.Typeable](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Typeable.html)
to generate documentation for [Servant](https://hackage.haskell.org/package/servant) API types.

It relies on the `typeRep` of Servant's combinators and custom types used in
the API to generate the documentation.

# Functionality

- Parses the API into a documentation friendly structure (see Servant.Docs.Simple.Parse)

- Renders the structure into common documentation formats (see Servant.Docs.Simple.Render)

- Provides functions to write rendered formats to file/stdout

# Tutorials

To run these:

``` sh
# clone this repository
git clone git@github.com:Holmusk/servant-docs-simple.git

# run the examples
stack examples/<source file>
```

![Generating plaintext/JSON documentation from api types](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/render.hs)

![Generating the intermediate documentation structure](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/parse.hs)

![Writing our own rendering format](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/format.hs)
