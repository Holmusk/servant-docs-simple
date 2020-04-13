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

# FAQ

- Why use this library when we already have `Servant.Docs`?
