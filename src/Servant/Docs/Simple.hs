{-| Simple documentation for API endpoints

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

--}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.Docs.Simple (collate, documentEndpoint) where

import Data.Proxy
import Data.String (fromString)
import Data.Text.Prettyprint.Doc (Doc, line, vcat)
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant.API ((:>), AuthProtect, BasicAuth, Capture', CaptureAll, Description, Header',
                    HttpVersion, IsSecure, QueryFlag, QueryParam', QueryParams, RemoteHost,
                    ReqBody', StreamBody', Summary, Vault, Verb)

-- | Folds api endpoints into documentation
class HasCollatable api where
    -- | Folds list of endpoints to documentation
    collate :: Doc ann

instance (HasDocumentApi api, HasCollatable b) => HasCollatable (api ': b) where
    collate = vcat' [ documentEndpoint @api
                    , line
                    , collate @b
                    ]

instance HasCollatable '[] where
    collate = ""

-- | Folds an api endpoint into documentation
documentEndpoint :: forall a ann. HasDocumentApi a => Doc ann
documentEndpoint = document @a "" ""

-- | Folds an api endpoint into documentation
class HasDocumentApi api where

    -- | We use this to destructure the API type and convert it into documentation
    document :: Doc ann -- ^ Route documentation
             -> Doc ann -- ^ Everything else documentation
             -> Doc ann -- ^ Generated documentation for the route

-- | Static route documentation
instance (HasDocumentApi b, KnownSymbol route) => HasDocumentApi ((route :: Symbol) :> b) where
    document r = document @b formatted
        where formatted = mconcat [r, "/", fragment]
              fragment = symbolVal' @route

-- | Capture documentation
instance (HasDocumentApi b, KnownSymbol dRoute, Typeable t) => HasDocumentApi (Capture' m (dRoute :: Symbol) t :> b) where
    document r = document @b formatted
        where formatted = mconcat [r, "/", "{", var, "::", format, "}"]
              var = symbolVal' @dRoute
              format = typeText @t

-- | CaptureAll documentation
instance (HasDocumentApi b, KnownSymbol dRoute, Typeable t) => HasDocumentApi (CaptureAll (dRoute :: Symbol) t :> b) where
    document r = document @b formatted
        where formatted = mconcat [r, "/", "{", var, "::", format, "}"]
              var = symbolVal' @dRoute
              format = typeText @t

-- | Request HttpVersion documentation
instance HasDocumentApi b => HasDocumentApi (HttpVersion :> b) where
    document r a = document @b r (vcat' [a, desc])
        where desc = "Captures Http Version: True"

-- | IsSecure documentation
instance HasDocumentApi b => HasDocumentApi (IsSecure :> b) where
    document r a = document @b r (vcat' [a, desc])
        where desc = "SSL Only: True"

-- | Request Remote host documentation
instance HasDocumentApi b => HasDocumentApi (RemoteHost :> b) where
    document r a = document @b r (vcat' [a, desc])
        where desc = "Captures RemoteHost/IP: True"

-- | Description documentation
instance (HasDocumentApi b, KnownSymbol desc) => HasDocumentApi (Description (desc :: Symbol) :> b) where
    document r a = document @b r (vcat' [a, desc])
        where desc = "Description: " <> symbolVal' @desc

-- | Summary documentation
instance (HasDocumentApi b, KnownSymbol s) => HasDocumentApi (Summary (s :: Symbol) :> b) where
    document r a = document @b r (vcat' [a, desc])
        where desc = "Summary: " <> symbolVal' @s

-- | Vault documentation
instance HasDocumentApi b => HasDocumentApi (Vault :> b) where
    document r a = document @b r (vcat' [a, desc])
        where desc = "Vault: True"

-- | Basic authentication documentation
instance (HasDocumentApi b, KnownSymbol realm, Typeable a) => HasDocumentApi (BasicAuth (realm :: Symbol) a :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = vcat' [ "Basic Authentication: "
                                , "  Realm: " <> realm
                                , "  UserData: " <> userData
                                ]
              realm = (symbolVal' @realm)
              userData = typeText @a

-- | Authentication documentation
instance (HasDocumentApi b, KnownSymbol token) => HasDocumentApi (AuthProtect (token :: Symbol) :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = "Authentication: " <> authDoc
              authDoc = symbolVal' @token

-- | Request header documentation
instance (HasDocumentApi b, KnownSymbol ct, Typeable typ) => HasDocumentApi (Header' m (ct :: Symbol) typ :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = vcat' [ "RequestHeaders:"
                                , "  Name: " <>  (symbolVal' @ct)
                                , "  ContentType: " <> typeText @typ
                                ]

-- | Query flag documentation
instance (HasDocumentApi b, KnownSymbol param) => HasDocumentApi (QueryFlag (param :: Symbol) :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = vcat' [ "QueryFlag:"
                                , "  Param: " <>  (symbolVal' @param)
                                ]

-- | Query param documentation
instance (HasDocumentApi b, KnownSymbol param, Typeable typ) => HasDocumentApi (QueryParam' m (param :: Symbol) typ :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = vcat' [ "QueryParam:"
                                , "  Param: " <>  (symbolVal' @param)
                                , "  ContentType: " <> typeText @typ
                                ]

-- | Query params documentation
instance (HasDocumentApi b, KnownSymbol param, Typeable typ) => HasDocumentApi (QueryParams (param :: Symbol) typ :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = vcat' [ "QueryParams:"
                                , "  Param: " <>  (symbolVal' @param)
                                , "  ContentType: " <> typeText @typ
                                ]

-- | Request body documentation
instance (HasDocumentApi b, Typeable ct, Typeable typ) => HasDocumentApi (ReqBody' m ct typ :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = vcat' [ "RequestBody:"
                                , "  Format: " <> typeText @ct
                                , "  ContentType: " <> typeText @typ
                                ]

-- | Stream body documentation
instance (HasDocumentApi b, Typeable ct, Typeable typ) => HasDocumentApi (StreamBody' m ct typ :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = vcat' [ "StreamBody:"
                                , "  Format: " <> typeText @ct
                                , "  ContentType: " <> typeText @typ
                                ]

-- | Response documentation
--   Terminates here as responses are last parts of api endpoints
--   Note that request type information (GET, POST etc...) is contained here
instance (Typeable m, Typeable ct, Typeable typ) => HasDocumentApi (Verb m s ct typ) where
    document r a = vcat' [r, a, formatted]
        where formatted = vcat' [ requestType
                                , response
                                ]
              requestType = "RequestType: " <> typeText @m
              response = vcat' [ "Response:"
                               , "  Format: " <> typeText @ct
                               , "  ContentType: " <> typeText @typ
                               ]

-- | Internal Helper utilities
typeText :: forall a ann. (Typeable a) => Doc ann
typeText = fromString $ show $ typeRep $ Proxy @a

vcat' :: [Doc ann] -> Doc ann
vcat' = vcat . filter ((/= "") . show)

symbolVal' :: forall n ann. KnownSymbol n => Doc ann
symbolVal' = fromString $ symbolVal $ Proxy @n
