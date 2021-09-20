# Changelog

[The latest version of this document is on GitHub.](https://github.com/Holmusk/servant-docs-simple/blob/master/CHANGELOG.md)

## 0.1.0.0

* Initially created.

## 0.2.0.0

* Use OMap as underlying representation for intermediate documentation structure. 

* Expose HasDocumentApi typeclass to allow custom API type combinator parsing.

## 0.2.0.1

* Update CHANGELOG, README

## 0.3.0.0

* Add markdown support

* Update typeclass names in `Servant.Docs.Simple.Parse`:

  `HasParsable` -> `HasParsableApi`
  
  `HasDocumentApi` -> `HasParsableEndpoint`
  
  `parse` -> `parseApi`

## 0.4.0.0

* Improve how type-level lists are rendered.
    Instead of "Format: ': * JSON ('[] *)"
    you'll get "Format: [JSON]"

* Fix bug when any given API combinator would only be rendered once per endpoint

* API change: removed `toDetails` function. Use `Details` constructor directly instead.
