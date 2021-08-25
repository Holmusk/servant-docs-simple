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

-}

{-# LANGUAGE UndecidableInstances #-}

module Servant.Docs.Simple.Parse
       ( HasParsableEndpoint (..)
       , HasParsableApi (..)
       , symbolVal'
       , toDetails
       , typeText
       , typeListText
       ) where


import Data.Foldable (fold)
import Data.Map.Ordered (OMap, empty, fromList, (|<))
import Data.Proxy
import Data.Text (Text, pack)
import Data.Typeable (Typeable, typeRep, TypeRep, splitTyConApp, TyCon, tyConPackage, tyConModule, tyConName)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Servant.API ((:>), AuthProtect, BasicAuth, Capture', CaptureAll, Description, EmptyAPI,
                    Header', HttpVersion, IsSecure, QueryFlag, QueryParam', QueryParams, RemoteHost,
                    ReqBody', StreamBody', Summary, Vault, Verb)
import qualified Servant.API.TypeLevel as S (Endpoints)

import Servant.Docs.Simple.Render (ApiDocs (..), Details (..), Parameter, Route)
import Data.Kind (Type)

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
instance (HasParsableEndpoint b, KnownSymbol ct, Typeable typ) => HasParsableEndpoint (Header' m (ct :: Symbol) typ :> b) where
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
instance (HasParsableEndpoint b, KnownSymbol param, Typeable typ) => HasParsableEndpoint (QueryParam' m (param :: Symbol) typ :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [( "QueryParam"
                                        , toDetails [ ("Param", Detail $ symbolVal' @param)
                                                    , ("ContentType", Detail $ typeText @typ)
                                                    ]
                                        )]

-- | Query params documentation
instance (HasParsableEndpoint b, KnownSymbol param, Typeable typ) => HasParsableEndpoint (QueryParams (param :: Symbol) typ :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [(  "QueryParams"
                                        , toDetails [ ("Param", Detail $ symbolVal' @param)
                                                    , ("ContentType", Detail $ typeText @typ)
                                                    ]
                                        )]

-- | Request body documentation
instance (HasParsableEndpoint b, Typeable (ct :: [Type]), Typeable typ) => HasParsableEndpoint (ReqBody' m (ct :: [Type]) typ :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [( "RequestBody"
                                        , toDetails [ ("Format", Detail $ typeListText @ct)
                                                    , ("ContentType", Detail $ typeText @typ)
                                                    ]
                                        )]

-- | Stream body documentation
instance (HasParsableEndpoint b, Typeable ct, Typeable typ) => HasParsableEndpoint (StreamBody' m ct typ :> b) where
    parseEndpoint r a = parseEndpoint @b r $ a <> [( "StreamBody"
                                        , toDetails [ ("Format", Detail $ typeText @ct)
                                                    , ("ContentType", Detail $ typeText @typ)
                                                    ]
                                        )]

-- | Response documentation
--   Terminates here as responses are last parts of api endpoints
--   Note that request type information (GET, POST etc...) is contained here
instance (Typeable m, Typeable (ct :: [Type]), Typeable typ) => HasParsableEndpoint (Verb m s (ct :: [Type]) typ) where
    parseEndpoint r a = ( r
                   , fromList $ a <> [requestType, response]
                   )
        where requestType = ("RequestType", Detail $ typeText @m)
              response = ( "Response"
                         , toDetails [ ("Format", Detail $ typeListText @ct)
                                     , ("ContentType", Detail $ typeText @typ)
                                     ]
                         )

-- | Convert parameter-value pairs to Details type
toDetails :: [(Text, Details)] -> Details
toDetails = Details . fromList

-- | Convert types to Text
typeText :: forall a. Typeable a => Text
typeText = pack . show . typeRep $ Proxy @a

-- | Converts type-level list of types to Text.
-- If the type variable doesn't correspond to type level list,
-- the result is the same as calling 'typeText'.
--
-- >>> typeListText @'[JSON,PlainText]
-- "[JSON,PlainText]"
---
-- This is nicer way to print type-level lists than using 'typeText',
-- the output of which is difficult to read due to use of ticked list constructors.
-- >>> typeText @'[JSON,PlainText]
-- "': * JSON (': * PlainText ('[] *))"
typeListText :: forall a. Typeable a => Text
typeListText = case go . typeRep $ Proxy @a of
    Nothing -> typeText @a
    Just typeReps -> pack $ show typeReps
  where
    go :: TypeRep -> Maybe [TypeRep]
    go typRep = case splitTyConApp typRep of
        (tyCon, [x,xs]) | isCons tyCon -> (x:) <$> go xs
        (tyCon, [])| isNil tyCon -> Just []
        _ -> Nothing
    
    isCons :: TyCon -> Bool
    isCons tc =
        tyConPackage tc == "ghc-prim"
        && tyConModule tc == "GHC.Types"
        && tyConName tc == "':"

    isNil :: TyCon -> Bool
    isNil tc =
        tyConPackage tc == "ghc-prim"
        && tyConModule tc == "GHC.Types"
        && tyConName tc == "'[]"

-- | Convert symbol to Text
symbolVal' :: forall n. KnownSymbol n => Text
symbolVal' = pack . symbolVal $ Proxy @n
