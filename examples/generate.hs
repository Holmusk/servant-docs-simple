-- stack --system-ghc runghc --package servant-docs-simple
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}


module Main where

import Data.Aeson (Value)
import Data.Text (Text)

import Servant.API ((:>), Post, ReqBody)
import Servant.Docs.Simple (document, documentWith, stdoutJson, stdoutMarkdown, stdoutPlainText,
                            writeDocsJson, writeDocsMarkdown, writeDocsPlainText)
import Servant.Docs.Simple.Render (Json (..), Markdown (..), PlainText (..))

-- Our API type
type API = "hello" :> "world" :> Request :> Response
type Request = ReqBody '[()] ()
type Response = Post '[()] ()

-- Render API as Text documentation
documentText :: Text
documentText = getPlainText $ document @API

-- Render API as Markdown documentation, using prettyprinter annotations
documentMarkdown :: Text
documentMarkdown = getMarkdown $ documentWith @API @Markdown

-- Render API as Json documentation, represented using Data.Aeson
documentJson :: Value
documentJson = getJson $ documentWith @API @Json


main :: IO ()
main = do
  stdoutPlainText @API                    -- Writes documentation as PlainText to stdout
  putStrLn "\n"
  stdoutMarkdown @API                     -- Writes documentation as Markdown to stdout
  putStrLn "\n"
  stdoutJson @API                         -- Writes documentation as Json to stdout

  writeDocsJson @API "docs.json"           -- Writes to the file $PWD/docs.json
  writeDocsPlainText @API "docs"           -- Writes to the file $PWD/docs
  writeDocsMarkdown @API "docs.md"         -- Writes to the file $PWD/docs.md
