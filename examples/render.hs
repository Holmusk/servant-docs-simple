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

-- Render API as Text documentation
documentText :: Text
documentText = getPlainText $ document @API

-- Render API as Json documentation, represented using Data.Aeson
documentJson :: Value
documentJson = getJson $ documentWith @API @Json

main :: IO ()
main = do
  stdoutPlainText @API               -- Writes documentation as PlainText to stdout
  putStrLn "\n"
  stdoutJson @API                    -- Writes documentation as Json to stdout

  writeDocsJson @API "docsJson"           -- Writes to the file $PWD/docsJson
  writeDocsPlainText @API "docsPlainText" -- Writes to the file $PWD/docsPlainText
