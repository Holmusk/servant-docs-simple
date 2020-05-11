{- | Parse and render an API type, write documentation to file, stdout

__Example script__

[Writing documentation to file](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/generate.hs)

/With the following language extensions/

> DataKinds
> TypeApplications
> TypeOperators

/Using this script/

> module Main where
>
> import Servant.API ((:>), Post, ReqBody)
> import Servant.Docs.Simple (writeDocsJson, writeDocsPlainText)
>
> -- Our API type
> type API = "hello" :> "world" :> Request :> Response
> type Request = ReqBody '[()] ()
> type Response = Post '[()] ()
>
> main :: IO ()
> main = do
>   -- Writes to the file $PWD/docsJson
>   writeDocsJson @API "docs.json"
>
>   -- Writes to the file $PWD/docsPlainText
>   writeDocsPlainText @API "docs.txt"

__Expected Output__

/Files should be generated relative to @$PWD@/

> $ ls | grep docs
> docs.json
> docs.txt

/docs.json/

> {
>     "/hello/world": {
>         "Response": {
>             "Format": "': * () ('[] *)",
>             "ContentType": "()"
>         },
>         "RequestType": "'POST",
>         "RequestBody": {
>             "Format": "': * () ('[] *)",
>             "ContentType": "()"
>         }
>     }
> }

/docs.txt/

> /hello/world:
> RequestBody:
>     Format: ': * () ('[] *)
>     ContentType: ()
> RequestType: 'POST
> Response:
>     Format: ': * () ('[] *)
>     ContentType: ()

-}

module Servant.Docs.Simple ( document
                           , documentWith
                           , stdoutJson
                           , stdoutPlainText
                           , writeDocsJson
                           , writeDocsPlainText
                           ) where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B (writeFile)
import qualified Data.ByteString.Lazy.Char8 as BC (putStrLn)
import qualified Data.Text.IO as T (putStrLn, writeFile)

import Servant.Docs.Simple.Parse (HasParsableApi (..))
import Servant.Docs.Simple.Render (Json (..), PlainText (..), Renderable (..))


-- | Write documentation as PlainText to file
writeDocsPlainText :: forall api. HasParsableApi api => FilePath -> IO ()
writeDocsPlainText fp = T.writeFile fp . getPlainText $ document @api

-- | Write documentation as JSON to file
writeDocsJson :: forall api. HasParsableApi api => FilePath -> IO ()
writeDocsJson fp = B.writeFile fp . encodePretty . getJson $ documentWith @api @Json

-- | Write documentation as PlainText to stdout
stdoutPlainText :: forall api. HasParsableApi api => IO ()
stdoutPlainText = T.putStrLn . getPlainText $ document @api

-- | Write documentation as JSON to stdout
stdoutJson :: forall api. HasParsableApi api => IO ()
stdoutJson = BC.putStrLn . encodePretty . getJson $ documentWith @api @Json

-- | Convert API type into PlainText format
document :: forall api. HasParsableApi api => PlainText
document = documentWith @api @PlainText

-- | Convert API type into specified formats
documentWith :: forall api a. (HasParsableApi api, Renderable a) => a
documentWith = render @a (parseApi @api)
