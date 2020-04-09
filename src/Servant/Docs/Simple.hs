{- | Simple documentation for API endpoints

TODO: Add language extensions used

Example:

@
    import Servant.Docs.Simple (collate)
    import Servant.API.Typelevel (Endpoints, ReqBody', Request)

    type Request = ReqBody '[()] ()
    type Response = Post '[()] ()
    type API = "hello" :> "world" :> Request :> Response

    documentation :: Text
    documentation = collate @(Endpoints API)
@
-}

module Servant.Docs.Simple ( document
                           , documentWith
                           , writeDocsJson
                           , writeDocsPlainText
                           ) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy as B (writeFile)
import Data.Text.IO as T (writeFile)
import Servant.Docs.Simple.Parse (HasParsable (..))
import Servant.Docs.Simple.Render ( Json (..)
                                  , PlainText (..)
                                  , Renderable (..)
                                  )


-- | Write documentation as PlainText to file
writeDocsPlainText :: forall api. HasParsable api => FilePath -> IO ()
writeDocsPlainText fp = T.writeFile fp . getPlainText $ document @api

-- | Write documentation as Json to file
writeDocsJson :: forall api. HasParsable api => FilePath -> IO ()
writeDocsJson fp = B.writeFile fp . encodePretty . getJson $ documentWith @api @Json

-- | Convert API type into PlainText format
document :: forall api. HasParsable api => PlainText
document = documentWith @api @PlainText

-- | Convert API type into specified formats
documentWith :: forall api a. (HasParsable api, Renderable a) => a
documentWith = render @a (parse @api)
