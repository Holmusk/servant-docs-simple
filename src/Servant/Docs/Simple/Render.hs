{- | Renders the intermediate structure into common documentation formats

__Example scripts__

[Generating plaintext/JSON documentation from api types](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/render.hs)

[Writing our own rendering format](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/format.hs)

__Example of rendering the intermediate structure__

/Intermediate structure/

> ApiDocs [Node "/hello/world"
>                 (Details [ Node "RequestBody" (Details [ Node "Format"
>                                                               (Detail "': * () ('[] *)")
>                                                        , Node "ContentType"
>                                                               (Detail "()")
>                                                        ])
>                          , Node "RequestType" (Detail "'POST")
>                          , Node "Response" (Details [ Node "Format"
>                                                            (Detail "': * () ('[] *)")
>                                                     , Node "ContentType"
>                                                            (Detail "()")
>                                                     ])
>                          ])]


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

module Servant.Docs.Simple.Render ( ApiDocs (..)
                                  , Details (..)
                                  , Renderable (..)
                                  , Parameter
                                  , Route
                                  , Json (..)
                                  , Pretty (..)
                                  , PlainText (..)
                                  ) where

import Data.Aeson (ToJSON (..), Value (..))
import Data.List (intersperse)
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (Doc, cat, line, nest, pretty, vcat, vsep)
import Data.Map.Ordered (OMap, assocs)
import Data.HashMap.Strict (fromList)

-- | Intermediate documentation structure, a hashmap of endpoints
--
-- API type:
--
-- >   type API = "users" :> (      "update" :> Response '[()] ()
-- >                           :<|> "get"    :> Response '[()] ()
-- >                         )
--
-- TODO update this example
-- Parsed into ApiDocs:
--
-- >   ApiDocs [ Node "/users/update"
-- >                    (Details [ Node "Response"
-- >                                    (Details [ Node "Format" (Detail "': * () ('[] *)")
-- >                                             , Node "ContentType" (Detail "()")
-- >                                             ])
-- >                             ])
-- >             , Node "/users/get"
-- >                    (Details [ Node "Response"
-- >                                    (Details [ Node "Format" (Detail "': * () ('[] *)")
-- >                                             , Node "ContentType" (Detail "()")
-- >                                             ])
-- >                             ])
-- >             ]
--
-- For a breakdown reference 'Node'
--
-- For more examples reference [Test.Servant.Docs.Simple.Samples](https://github.com/Holmusk/servant-docs-simple/blob/master/test/Test/Servant/Docs/Simple/Samples.hs)
--
newtype ApiDocs = ApiDocs (OMap Route Details) deriving stock (Eq, Show)

-- | Route representation
type Route = Text

-- TODO update these examples
-- | Key-Value pair for endpoints using the route as the key and parameters as the values
--
-- __Example 1__
--
-- An endpoint is represented as a node, with the route as its parameter and its Details as its value
--
-- > Node "/users/get" <Details>
--
-- __Example 2__
--
-- Details of each endpoint can also be represented as nodes
--
-- Given the following:
--
-- > Response '[()] ()
--
-- This can be interpreted as a Response parameter, with a value of 2 Details, Format and ContentType
--
-- In turn, this:
--
-- > Format: '[()]
--
-- can be interpreted as a Format parameter with a value of @'[()]@.
--
-- And so parsing @Response '[()] ()@ comes together as:
--
-- > Node "Response"                                               --- Parameter
-- >      (Details [ Node "Format"                   -- Parameter  ---
-- >                      (Detail "': * () ('[] *)") -- Value         |
-- >               , Node "ContentType"              -- Parameter     | Value
-- >                      (Detail "()")              -- Value         |
-- >               ])                                              ---
--

-- TODO include how this looks
-- | Details of the Api Route; see 'ApiDocs' documentation for a clearer picture
data Details = Details (OMap Parameter Details) -- ^ List of Parameter-Value pairs
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
    toJSON (Detail t) = String t
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
instance Renderable PlainText where
    render = PlainText . pack . show . getPretty . render
