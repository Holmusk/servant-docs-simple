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

module Servant.Docs.Simple (HasCollatable, HasDocumentApi, collate, documentEndpoint) where

import Data.Proxy
import Data.String (fromString)
import Data.Text.Prettyprint.Doc (Doc, line, vcat)
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant.API ((:>), AuthProtect, Capture', Header', QueryFlag, QueryParam', QueryParams,
                    ReqBody', Verb)

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
              fragment = symbolVal' $ Proxy @route

-- | Dynamic route documentation
instance (HasDocumentApi b, KnownSymbol dRoute, Typeable t) => HasDocumentApi (Capture' m (dRoute :: Symbol) t :> b) where
    document r = document @b formatted
        where formatted = mconcat [r, "/", "{", var, "::", format, "}"]
              var = symbolVal' $ Proxy @dRoute
              format = typeText (Proxy @t)

-- | Authentication documentation
instance (HasDocumentApi b, KnownSymbol token) => HasDocumentApi (AuthProtect (token :: Symbol) :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = "Authentication: " <> authDoc
              authDoc = symbolVal' (Proxy @token)

-- | Request header documentation
instance (HasDocumentApi b, KnownSymbol ct, Typeable typ) => HasDocumentApi (Header' m (ct :: Symbol) typ :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = vcat' [ "RequestHeaders:"
                               , "  Name: " <>  (symbolVal' $ Proxy @ct)
                               , "  ContentType: " <> typeText (Proxy @typ)
                               ]

-- | Query flag documentation
instance (HasDocumentApi b, KnownSymbol param) => HasDocumentApi (QueryFlag (param :: Symbol) :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = vcat' [ "QueryFlag:"
                               , "  Param: " <>  (symbolVal' $ Proxy @param)
                               ]

-- | Query param documentation
instance (HasDocumentApi b, KnownSymbol param, Typeable typ) => HasDocumentApi (QueryParam' m (param :: Symbol) typ :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = vcat' [ "QueryParam:"
                               , "  Param: " <>  (symbolVal' $ Proxy @param)
                               , "  ContentType: " <> typeText (Proxy @typ)
                               ]

-- | Query params documentation
instance (HasDocumentApi b, KnownSymbol param, Typeable typ) => HasDocumentApi (QueryParams (param :: Symbol) typ :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = vcat' [ "QueryParams:"
                               , "  Param: " <>  (symbolVal' $ Proxy @param)
                               , "  ContentType: " <> typeText (Proxy @typ)
                               ]

-- | Request body documentation
instance (HasDocumentApi b, Typeable ct, Typeable typ) => HasDocumentApi (ReqBody' m ct typ :> b) where
    document r a = document @b r (vcat' [a, formatted])
        where formatted = vcat' [ "RequestBody:"
                               , "  Format: " <> typeText (Proxy @ct)
                               , "  ContentType: " <> typeText (Proxy @typ)
                               ]

-- | Response documentation
--   Terminates here as responses are last parts of api endpoints
--   Note that request type information (GET, POST etc...) is contained here
instance (Typeable m, Typeable ct, Typeable typ) => HasDocumentApi (Verb m s ct typ) where
    document r a = vcat' [r, a, formatted]
        where formatted = vcat' [ requestType
                               , response
                               ]
              requestType = "RequestType: " <> typeText (Proxy @m)
              response = vcat' [ "Response:"
                              , "  Format: " <> typeText (Proxy @ct)
                              , "  ContentType: " <> typeText (Proxy @typ)
                              ]
              -- Remove empty parts of the documentation

typeText :: forall a ann. (Typeable a) => Proxy a -> Doc ann
typeText = fromString . show . typeRep

vcat' :: [Doc ann] -> Doc ann
vcat' = vcat . filter ((/= "") . show)

symbolVal' :: KnownSymbol n => proxy n -> Doc ann
symbolVal' = fromString . symbolVal
