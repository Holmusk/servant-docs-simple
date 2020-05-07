-- stack --system-ghc runghc --package servant-docs-simple
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}


module Main where

import Servant.API ((:>), Post, ReqBody)
import Servant.Docs.Simple (writeDocsJson, writeDocsPlainText)

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
