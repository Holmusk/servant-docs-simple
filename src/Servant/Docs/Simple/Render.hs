{- | Renders the intermediate structure into common documentation formats

__Example scripts__

[Generating plaintext/JSON documentation from api types](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/generate.hs)

[Writing our own rendering format](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/render.hs)

__Example of rendering the intermediate structure__

/Intermediate structure/

> ApiDocs ( fromList [( "/hello/world",
>                     , Details (fromList ([ ( "RequestBody"
>                                            , Details (fromList ([ ( "Format"
>                                                                   , Detail "': * () ('[] *)"
>                                                                   )
>                                                                 , ( "ContentType"
>                                                                   , Detail "()"
>                                                                   )
>                                                                 ]))
>                                            )
>                                          , ( "RequestType"
>                                            , Detail "'POST"
>                                            )
>                                          , ( "Response"
>                                            , Details (fromList ([ ( "Format"
>                                                                   , Detail "': * () ('[] *)"
>                                                                   )
>                                                                 , ( "ContentType"
>                                                                   , Detail "()"
>                                                                   )
>                                                                 ]))
>                                            )
>                                          ]))
>                     )])


/JSON/

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

/Text/

> /hello/world:
> RequestBody:
>     Format: ': * () ('[] *)
>     ContentType: ()
> RequestType: 'POST
> Response:
>     Format: ': * () ('[] *)
>     ContentType: ()

-}

module Servant.Docs.Simple.Render
       ( ApiDocs (..)
       , Details (..)
       , Renderable (..)
       , Parameter
       , Route
       , Json (..)
       , Pretty (..)
       , PlainText (..)
       ) where

import Data.Aeson (ToJSON (..), Value (..))
import Data.HashMap.Strict (fromList)
import Data.List (intersperse)
import Data.Map.Ordered (OMap, assocs)
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (Doc, cat, line, nest, pretty, vcat, vsep)

-- | Intermediate documentation structure, a hashmap of endpoints
--
-- API type:
--
-- >   type API = "users" :> (      "update" :> Response '[()] ()
-- >                           :<|> "get"    :> Response '[()] ()
-- >                         )
--
-- Parsed into ApiDocs:
--
--
-- > ApiDocs ( fromList [ ( "/users/update",
-- >                      , Details (fromList ([ ( "Response"
-- >                                             , Details (fromList ([ ( "Format"
-- >                                                                    , Detail "': * () ('[] *)"
-- >                                                                    )
-- >                                                                  , ( "ContentType"
-- >                                                                    , Detail "()"
-- >                                                                    )
-- >                                                                 ]))
-- >                                             )
-- >                                           ]))
-- >                      )
-- >                    , ( "/users/get",
-- >                      , Details (fromList ([ ( "Response"
-- >                                             , Details (fromList ([ ( "Format"
-- >                                                                    , Detail "': * () ('[] *)"
-- >                                                                    )
-- >                                                                  , ( "ContentType"
-- >                                                                    , Detail "()"
-- >                                                                    )
-- >                                                                  ]))
-- >                                             )
-- >                                           ]))
-- >                     )
-- >                    ])
--
-- For more examples reference [Test.Servant.Docs.Simple.Samples](https://github.com/Holmusk/servant-docs-simple/blob/master/test/Test/Servant/Docs/Simple/Samples.hs)
--
newtype ApiDocs = ApiDocs (OMap Route Details) deriving stock (Eq, Show)

-- | Route representation
type Route = Text

-- | Details of the Api Route
--
-- __Examples__
--
-- > Authentication: true
--
-- Can be interpreted as a Parameter (Authentication) and a /Detail/ (true)
--
-- > Response:
-- >   Format: ...
-- >   ContentType: ...
--
-- Can be interpreted as a Parameter (Response) and /Details/ (Format (...), ContentType (...))
--
data Details = Details (OMap Parameter Details) -- ^ OMap of Parameter-Details
             | Detail Text    -- ^ Single Value
             deriving stock (Eq, Show)

-- | Parameter names
type Parameter = Text

-- | Convert ApiDocs into different documentation formats
class Renderable a where
    render :: ApiDocs -> a

-- | Conversion to JSON using Data.Aeson
newtype Json = Json { getJson :: Value } deriving stock (Eq, Show)

-- | Conversion to JSON using Data.Aeson
instance Renderable Json where
    render = Json . toJSON

-- | Json instance for the endpoints hashmap
instance ToJSON ApiDocs where
    toJSON (ApiDocs endpoints) = toJSON . fromList . assocs $ endpoints

-- | Json instance for the parameter hashmap of each endpoint
instance ToJSON Details where
    toJSON (Detail t)   = String t
    toJSON (Details ls) = toJSON . fromList . assocs $ ls

-- | Conversion to prettyprint
newtype Pretty ann = Pretty { getPretty :: Doc ann }

-- | Conversion to prettyprint
instance Renderable (Pretty ann) where
    render = Pretty . prettyPrint

-- | Helper function to prettyprint the ApiDocs
prettyPrint :: ApiDocs -> Doc ann
prettyPrint (ApiDocs endpoints) = vcat . intersperse line
                                $ uncurry (toDoc 0) <$> assocs endpoints

-- | Helper function
toDoc :: Int -> Text -> Details -> Doc ann
toDoc i t d = case d of
    Detail a   -> cat [pretty t, ": ", pretty a]
    Details as -> nest i . vsep $ pretty t <> ":"
                                : (uncurry (toDoc (i + 4)) <$> assocs as)

-- | Conversion to plaintext
newtype PlainText = PlainText { getPlainText :: Text } deriving stock (Eq, Show)

-- | Conversion to plaintext
instance Renderable PlainText where
    render = PlainText . pack . show . getPretty . render
