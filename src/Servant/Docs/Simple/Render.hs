{-# LANGUAGE CPP #-}
{- | Renders the intermediate structure into common documentation formats

__Example scripts__

[Generating plaintext/JSON documentation from api types](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/generate.hs)

[Writing our own rendering format](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/render.hs)

__Example of rendering the intermediate structure__

/Intermediate structure/

> ApiDocs ( fromList [( "/hello/world",
>                     , Details (fromList ([ ( "RequestBody"
>                                            , Details (fromList ([ ( "Format"
>                                                                   , Detail "[()]"
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
>                                                                   , Detail "[()]"
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
>             "Format": "[()]",
>             "ContentType": "()"
>         },
>         "RequestType": "'POST",
>         "RequestBody": {
>             "Format": "[()]",
>             "ContentType": "()"
>         }
>     }
> }

/Text/

> /hello/world:
> RequestBody:
>     Format: [()]
>     ContentType: ()
> RequestType: 'POST
> Response:
>     Format: [()]
>     ContentType: ()

-}

module Servant.Docs.Simple.Render
       ( ApiDocs (..)
       , Details (..)
       , Renderable (..)
       , Parameter
       , Route
       , Json (..)
       , Markdown (..)
       , Pretty (..)
       , PlainText (..)
       ) where

import Data.Aeson (ToJSON (..), Value (..))
import Data.HashMap.Strict (fromList)
import Data.List (intersperse)
import Data.Map.Ordered (OMap, assocs)
import Data.Text (Text, pack)
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter (Doc, annotate, defaultLayoutOptions, indent, layoutPretty, line, pretty, vcat,
                      vsep)
import Prettyprinter.Render.Util.StackMachine (renderSimplyDecorated)
#else
import Data.Text.Prettyprint.Doc (Doc, annotate, defaultLayoutOptions, indent, layoutPretty, line,
                                  pretty, vcat, vsep)
import Data.Text.Prettyprint.Doc.Render.Util.StackMachine (renderSimplyDecorated)
#endif

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
-- >                                                                    , Detail "[()]"
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
-- >                                                                    , Detail "[()]"
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
newtype Pretty = Pretty { getPretty :: Doc Ann }

-- | Annotates our route and parameter keys
data Ann = AnnRoute | AnnParam | AnnDetail

-- | Conversion to prettyprint
instance Renderable Pretty where
    render = Pretty . prettyPrint

-- | Helper function to prettyprint the ApiDocs
prettyPrint :: ApiDocs -> Doc Ann
prettyPrint (ApiDocs endpoints) = vsep
                                $ intersperse line
                                $ documentRoute
                              <$> assocs endpoints

-- | Documents an API route
documentRoute :: (Route, Details) -- ^ Route-Details pair
               -> Doc Ann -- ^ documentation for Route-Details pair
documentRoute (r, d) = routeDoc <> ":" <> detailsDoc
  where routeDoc = annotate AnnRoute $ pretty r
        detailsDoc = documentDetails 0 d

-- | Documents Details of an API route
documentDetails :: Int -- ^ Indentation
                -> Details -- ^ Details
                -> Doc Ann -- ^ documentation for Details
documentDetails i d = case d of
    Detail d'  -> " " <> annotate AnnDetail (pretty d')
    Details ds -> (line <>)
                $ indent i
                $ vcat
                $ documentParameters <$> ds'
      where ds' = assocs ds
            documentParameters (param, details) = annotate AnnParam (pretty param)
                                               <> ":"
                                               <> documentDetails (i + 4) details

-- | Conversion to plaintext
newtype PlainText = PlainText { getPlainText :: Text } deriving stock (Eq, Show)

-- | Conversion to plaintext
instance Renderable PlainText where
    render = PlainText . pack . show . getPretty . render

-- | Conversion to markdown
newtype Markdown = Markdown { getMarkdown :: Text } deriving stock (Eq, Show)

instance Renderable Markdown where
    render docs = Markdown m
      where m = renderSimplyDecorated id annOpen annClose docStream
            annOpen = \case
              AnnRoute  -> "### "
              AnnParam  -> "- **"
              AnnDetail -> "`"
            annClose = \case
              AnnRoute  -> ""
              AnnParam  -> "**"
              AnnDetail -> "`"
            docStream = layoutPretty defaultLayoutOptions docs'
            docs' = getPretty $ render docs
