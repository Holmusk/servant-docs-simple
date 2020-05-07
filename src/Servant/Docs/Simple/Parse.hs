{- | Parse Servant API into documentation

__Example script__

[Generating the intermediate documentation structure](https://github.com/Holmusk/servant-docs-simple/blob/master/examples/parse.hs)

__Example of parsing an API__

/API type/

> type API = "hello" :> "world" :> Request :> Response
> type Request = ReqBody '[()] ()
> type Response = Post '[()] ()

/Intermediate structure/

> Endpoints [Node "/hello/world"
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

-}

{-# LANGUAGE UndecidableInstances #-}

module Servant.Docs.Simple.Parse
       ( HasDocumentApi (..)
       , HasParsable (..)
       , toDetails
       ) where


import Data.Foldable (fold)
import Data.Map.Ordered (OMap, (|<), empty, fromList)
import Data.Proxy
import Data.Text (Text, pack)
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Servant.API ((:>), AuthProtect, BasicAuth, Capture', CaptureAll, Description, EmptyAPI,
                    Header', HttpVersion, IsSecure, QueryFlag, QueryParam', QueryParams, RemoteHost,
                    ReqBody', StreamBody', Summary, Vault, Verb)
import qualified Servant.API.TypeLevel as S (Endpoints)

import Servant.Docs.Simple.Render (ApiDocs (..), Details (..), Parameter, Route)

-- | Flattens API into type level list of 'Endpoints'
class HasParsable api where
    parse :: ApiDocs

instance HasCollatable (S.Endpoints a) => HasParsable a where
    parse = collate @(S.Endpoints a)

instance {-# OVERLAPPING #-} HasParsable EmptyAPI where
    parse = collate @'[]

-- | Folds api endpoints into documentation
class HasCollatable api where
    -- | Folds list of endpoints to documentation
    collate :: ApiDocs

instance (HasDocumentApi api, HasCollatable b) => HasCollatable (api ': b) where
    collate = ApiDocs $ (Details <$> documentEndpoint @api) |< previous
      where ApiDocs previous = collate @b

instance HasCollatable '[] where
    collate = ApiDocs empty

-- | Folds an api endpoint into documentation
documentEndpoint :: forall a. HasDocumentApi a => (Route, OMap Parameter Details)
documentEndpoint = document @a "" []

-- | Folds an api endpoint into documentation
class HasDocumentApi api where

    -- | We use this to destructure the API type and convert it into documentation
    document :: Route -- ^ Route documentation
             -> [(Parameter, Details)] -- ^ Everything else documentation
             -> (Route, OMap Parameter Details) -- ^ Generated documentation for the route

-- | Static route documentation
instance (HasDocumentApi b, KnownSymbol route) => HasDocumentApi ((route :: Symbol) :> b) where
    document r = document @b formatted
        where formatted = fold [r, "/", fragment]
              fragment = symbolVal' @route

-- | Capture documentation
instance (HasDocumentApi b, KnownSymbol dRoute, Typeable t) => HasDocumentApi (Capture' m (dRoute :: Symbol) t :> b) where
    document r = document @b formatted
        where formatted = fold [r, "/", "{", var, "::", format, "}"]
              var = symbolVal' @dRoute
              format = typeText @t

-- | CaptureAll documentation
instance (HasDocumentApi b, KnownSymbol dRoute, Typeable t) => HasDocumentApi (CaptureAll (dRoute :: Symbol) t :> b) where
    document r = document @b formatted
        where formatted = fold [r, "/", "{", var, "::", format, "}"]
              var = symbolVal' @dRoute
              format = typeText @t

-- | Request HttpVersion documentation
instance HasDocumentApi b => HasDocumentApi (HttpVersion :> b) where
    document r a = document @b r $ a <> [("Captures Http Version", Detail "True")]

-- | IsSecure documentation
instance HasDocumentApi b => HasDocumentApi (IsSecure :> b) where
    document r a = document @b r $ a <> [("SSL Only", Detail "True")]

-- | Request Remote host documentation
instance HasDocumentApi b => HasDocumentApi (RemoteHost :> b) where
    document r a = document @b r $ a <> [("Captures RemoteHost/IP", Detail "True")]

-- | Description documentation
instance (HasDocumentApi b, KnownSymbol desc) => HasDocumentApi (Description (desc :: Symbol) :> b) where
    document r a = document @b r $ a <> [("Description", Detail $ symbolVal' @desc)]

-- | Summary documentation
instance (HasDocumentApi b, KnownSymbol s) => HasDocumentApi (Summary (s :: Symbol) :> b) where
    document r a = document @b r $ a <> [("Summary", Detail $ symbolVal' @s)]

-- | Vault documentation
instance HasDocumentApi b => HasDocumentApi (Vault :> b) where
    document r a = document @b r $ a <> [("Vault", Detail "True")]

-- | Basic authentication documentation
instance (HasDocumentApi b, KnownSymbol realm, Typeable a) => HasDocumentApi (BasicAuth (realm :: Symbol) a :> b) where
    document r a = document @b r $ a <> [( "Basic Authentication"
                                        , (toDetails [ ("Realm", Detail realm)
                                                            , ("UserData", Detail userData)
                                                            ])
                                        )]

        where realm = symbolVal' @realm
              userData = typeText @a

-- | Authentication documentation
instance (HasDocumentApi b, KnownSymbol token) => HasDocumentApi (AuthProtect (token :: Symbol) :> b) where
    document r a = document @b r $ a <> [("Authentication", (Detail authDoc))]
        where authDoc = symbolVal' @token

-- | Request header documentation
instance (HasDocumentApi b, KnownSymbol ct, Typeable typ) => HasDocumentApi (Header' m (ct :: Symbol) typ :> b) where
    document r a = document @b r $ a <> [( "RequestHeaders"
                                        , (toDetails [ ("Name", Detail $ symbolVal' @ct)
                                                              , ("ContentType", Detail $ typeText @typ)
                                                              ])
                                        )]

-- | Query flag documentation
instance (HasDocumentApi b, KnownSymbol param) => HasDocumentApi (QueryFlag (param :: Symbol) :> b) where
    document r a = document @b r $ a <> [( "QueryFlag"
                                        , (toDetails [ ("Param", Detail $ symbolVal' @param) ])
                                        )]

-- | Query param documentation
instance (HasDocumentApi b, KnownSymbol param, Typeable typ) => HasDocumentApi (QueryParam' m (param :: Symbol) typ :> b) where
    document r a = document @b r $ a <> [( "QueryParam"
                                        , (toDetails [ ("Param", Detail $ symbolVal' @param)
                                                              , ("ContentType", Detail $ typeText @typ)
                                                              ])
                                        )]

-- | Query params documentation
instance (HasDocumentApi b, KnownSymbol param, Typeable typ) => HasDocumentApi (QueryParams (param :: Symbol) typ :> b) where
    document r a = document @b r $ a <> [(  "QueryParams"
                                        ,  (toDetails [ ("Param", Detail $ symbolVal' @param)
                                                                , ("ContentType", Detail $ typeText @typ)
                                                                ])
                                        )]

-- | Request body documentation
instance (HasDocumentApi b, Typeable ct, Typeable typ) => HasDocumentApi (ReqBody' m ct typ :> b) where
    document r a = document @b r $ a <> [( "RequestBody"
                                        , (toDetails [ ("Format", Detail $ typeText @ct)
                                                              , ("ContentType", Detail $ typeText @typ)
                                                              ])
                                        )]

-- | Stream body documentation
instance (HasDocumentApi b, Typeable ct, Typeable typ) => HasDocumentApi (StreamBody' m ct typ :> b) where
    document r a = document @b r $ a <> [( "StreamBody"
                                        , (toDetails [ ("Format", Detail $ typeText @ct)
                                                              , ("ContentType", Detail $ typeText @typ)
                                                              ])
                                        )]

-- | Response documentation
--   Terminates here as responses are last parts of api endpoints
--   Note that request type information (GET, POST etc...) is contained here
instance (Typeable m, Typeable ct, Typeable typ) => HasDocumentApi (Verb m s ct typ) where
    document r a = ( r
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

-- | Internal Helper utilities
typeText :: forall a. (Typeable a) => Text
typeText = pack . show . typeRep $ Proxy @a

symbolVal' :: forall n. KnownSymbol n => Text
symbolVal' = pack . symbolVal $ Proxy @n
