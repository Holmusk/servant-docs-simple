{- | Parse Servant API into documentation

__Example script__

[Generating the intermediate documentation structure](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/parse.hs)

[Parsing custom API type combinators](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/custom.hs)

__Example of parsing an API__

/API type/

> type API = "hello" :> "world" :> Request :> Response
> type Request = ReqBody '[()] ()
> type Response = Post '[()] ()

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

-}

{-# LANGUAGE UndecidableInstances #-}

module Servant.Docs.Simple.Parse
       ( HasParsableEndpoint (..)
       , HasParsableApi (..)
       , symbolVal'
       , toDetails
       , typeText
       ) where


import Data.Foldable (fold)
import Data.Kind (Type)
import Data.List (intersperse)
import Data.Map.Ordered (OMap, empty, fromList, (|<))
import Data.Proxy
import Data.Text (Text, pack)
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Servant.API ((:>), AuthProtect, BasicAuth, Capture', CaptureAll, Description, EmptyAPI,
                    Header', HttpVersion, IsSecure, QueryFlag, QueryParam', QueryParams, RemoteHost,
                    ReqBody', StdMethod, StreamBody', Summary, Vault, Verb)
import qualified Servant.API.TypeLevel as S (Endpoints)

import Servant.Docs.Simple.Render (ApiDocs (..), Details (..), Parameter, Route)

-- | Flattens API into type level list of Endpoints
class HasParsableApi api where
    parseApi :: ApiDocs

-- | If the flattened API can be collated into documentation, it is parsable
instance HasCollatable (S.Endpoints a) => HasParsableApi a where
    parseApi = collate @(S.Endpoints a)

-- | Empty APIs should have no documentation
instance {-# OVERLAPPING #-} HasParsableApi EmptyAPI where
    parseApi = collate @'[]

-- | Folds api endpoints into documentation
class HasCollatable api where
    -- | Folds list of endpoints to documentation
    collate :: ApiDocs

-- | Collapse a type-level list of API endpoints into documentation
instance (HasParsableEndpoint e, HasCollatable b) => HasCollatable (e ': b) where
    collate = ApiDocs $ (Details <$> documentEndpoint @e) |< previous
      where ApiDocs previous = collate @b

-- | Terminal step when there are no more endpoints left to recurse over
instance HasCollatable '[] where
    collate = ApiDocs empty

-- | Folds an api endpoint into documentation
documentEndpoint :: forall a. HasParsableEndpoint a => (Route, OMap Parameter Details)
documentEndpoint = parseEndpoint @a "" []

-- | Folds an api endpoint into documentation
class HasParsableEndpoint e where

    -- | We use this to destructure the API type and convert it into documentation
    parseEndpoint :: Route -- ^ Route documentation
                  -> [(Parameter, Details)] -- ^ Everything else documentation
                  -> (Route, OMap Parameter Details) -- ^ Generated documentation for the route

-- | Static route documentation
instance (HasParsableEndpoint b, KnownSymbol route) => HasParsableEndpoint ((route :: Symbol) :> b) where
    parseEndpoint r = parseEndpoint @b formatted
      where formatted = fold [r, "/", fragment]
            fragment = symbolVal' @route

-- | Capture documentation
instance (HasParsableEndpoint b, KnownSymbol dRoute, Typeable t) => HasParsableEndpoint (Capture' m (dRoute :: Symbol) t :> b) where
    parseEndpoint r = parseEndpoint @b formatted
      where formatted = fold [r, "/", "{", var, "::", format, "}"]
            var = symbolVal' @dRoute
            format = typeText @t

-- | CaptureAll documentation
instance (HasParsableEndpoint b, KnownSymbol dRoute, Typeable t) => HasParsableEndpoint (CaptureAll (dRoute :: Symbol) t :> b) where
    parseEndpoint r = parseEndpoint @b formatted
      where formatted = fold [r, "/", "{", var, "::", format, "}"]
            var = symbolVal' @dRoute
            format = typeText @t

-- | Request HttpVersion documentation
instance HasParsableEndpoint b => HasParsableEndpoint (HttpVersion :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [("Captures Http Version", Detail "True")]

-- | IsSecure documentation
instance HasParsableEndpoint b => HasParsableEndpoint (IsSecure :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [("SSL Only", Detail "True")]

-- | Request Remote host documentation
instance HasParsableEndpoint b => HasParsableEndpoint (RemoteHost :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [("Captures RemoteHost/IP", Detail "True")]

-- | Description documentation
instance (HasParsableEndpoint b, KnownSymbol desc) => HasParsableEndpoint (Description (desc :: Symbol) :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [("Description", Detail $ symbolVal' @desc)]

-- | Summary documentation
instance (HasParsableEndpoint b, KnownSymbol s) => HasParsableEndpoint (Summary (s :: Symbol) :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [("Summary", Detail $ symbolVal' @s)]

-- | Vault documentation
instance HasParsableEndpoint b => HasParsableEndpoint (Vault :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [("Vault", Detail "True")]

-- | Basic authentication documentation
instance (HasParsableEndpoint b, KnownSymbol realm, Typeable a) => HasParsableEndpoint (BasicAuth (realm :: Symbol) a :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [( "Basic Authentication"
                                        , toDetails [ ("Realm", Detail realm)
                                                    , ("UserData", Detail userData)
                                                    ]
                                        )]

        where realm = symbolVal' @realm
              userData = typeText @a

-- | Authentication documentation
instance (HasParsableEndpoint b, KnownSymbol token) => HasParsableEndpoint (AuthProtect (token :: Symbol) :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [("Authentication", Detail authDoc)]
        where authDoc = symbolVal' @token

-- | Request header documentation
instance (HasParsableEndpoint b, KnownSymbol ct, HasTypeText typ) => HasParsableEndpoint (Header' m (ct :: Symbol) typ :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [( "RequestHeaders"
                                        , toDetails [ ("Name", Detail $ symbolVal' @ct)
                                                    , ("ContentType", Detail $ typeText @typ)
                                                    ]
                                        )]

-- | Query flag documentation
instance (HasParsableEndpoint b, KnownSymbol param) => HasParsableEndpoint (QueryFlag (param :: Symbol) :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [( "QueryFlag"
                                        , toDetails [ ("Param", Detail $ symbolVal' @param) ]
                                        )]

-- | Query param documentation
instance (HasParsableEndpoint b, KnownSymbol param, HasTypeText typ) => HasParsableEndpoint (QueryParam' m (param :: Symbol) typ :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [( "QueryParam"
                                        , toDetails [ ("Param", Detail $ symbolVal' @param)
                                                    , ("ContentType", Detail $ typeText @typ)
                                                    ]
                                        )]

-- | Query params documentation
instance (HasParsableEndpoint b, KnownSymbol param, HasTypeText typ) => HasParsableEndpoint (QueryParams (param :: Symbol) typ :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [(  "QueryParams"
                                        , toDetails [ ("Param", Detail $ symbolVal' @param)
                                                    , ("ContentType", Detail $ typeText @typ)
                                                    ]
                                        )]

-- | Request body documentation
instance (HasParsableEndpoint b, HasTypeText ct, Typeable typ) => HasParsableEndpoint (ReqBody' m ct typ :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [( "RequestBody"
                                        , toDetails [ ("Format", Detail $ typeText @ct)
                                                    , ("ContentType", Detail $ typeText @typ)
                                                    ]
                                        )]

-- | Stream body documentation
instance (HasParsableEndpoint b, HasTypeText ct, Typeable typ) => HasParsableEndpoint (StreamBody' m ct typ :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [( "StreamBody"
                                        , toDetails [ ("Format", Detail $ typeText @ct)
                                                    , ("ContentType", Detail $ typeText @typ)
                                                    ]
                                        )]

-- | Response documentation
--   Terminates here as responses are last parts of api endpoints
--   Note that request type information (GET, POST etc...) is contained here
instance (HasTypeText m, HasTypeText ct, Typeable typ) => HasParsableEndpoint (Verb m s ct typ) where
    parseEndpoint r a = ( r
                   , fromList $ a <> [requestType, response]
                   )
        where requestType = ("RequestType", Detail $ typeText @m)
              response = ( "Response"
                         , toDetails [ ("Format", Detail $ typeText @ct)
                                     , ("ContentType", Detail $ typeText @typ)
                                     ]
                         )


-- | Convert parameter-value pairs to Details type
toDetails :: [(Text, Details)] -> Details
toDetails = Details . fromList

-- | Convert symbol to Text
symbolVal' :: forall n. KnownSymbol n => Text
symbolVal' = pack . symbolVal $ Proxy @n

-- | Convert types to Text
class HasTypeText t where
    -- | Convert types to Text
    typeText :: Text

instance HasTypeText ('[] :: [Type]) where
    typeText = ""

instance (HasListText t) => HasTypeText (t :: [Type]) where
    typeText = listText @t []

instance Typeable t => HasTypeText (t :: Type) where
    typeText = pack . show . typeRep $ Proxy @t

instance Typeable t => HasTypeText (t :: StdMethod) where
    typeText = pack . show . typeRep $ Proxy @t

-- | Prettyprint type-level lists
class HasListText t where
    -- | Convert type-lists to Text
    listText :: [Text]
             -> Text

instance HasListText ('[] :: [Type]) where
    listText ts = "[" <> fold (intersperse ", " ts) <> "]"

instance (HasTypeText t, HasListText ts) => HasListText ((t ': ts) :: [Type]) where
    listText tx = listText @ts $ tx <> [typeText @t]

-- TODO Expose the typeText interface
-- Update tests
