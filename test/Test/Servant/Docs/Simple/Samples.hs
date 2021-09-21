-- | Contains sample Servant API types and their formatted counterparts
{-# LANGUAGE QuasiQuotes #-}

module Test.Servant.Docs.Simple.Samples (ApiComplete, ApiMultiple, apiCompletePlainText,
                                         apiCompleteParsed, apiCompleteJson, apiMultipleParsed) where

import Data.Aeson (Value (String), object)
import Servant.API (AuthProtect, BasicAuth, Capture, CaptureAll, Description, Header, HttpVersion,
                    IsSecure,QueryFlag, QueryParam, QueryParams, RemoteHost, ReqBody,
                    StreamBody, Summary, Vault, (:<|>), (:>), JSON )
import Servant.API.Verbs (Put, Post, Patch, Get)

import Text.RawString.QQ (r)

import Servant.Docs.Simple.Render (ApiDocs (..), Details (..), Json (..), PlainText (..))


type ApiComplete = StaticRouteTest :> DynRouteTest :> CaptureAllTest :> ApiDetails

apiCompleteParsed :: ApiDocs
apiCompleteParsed = ApiDocs [("/test_route/{test::()}/{test::()}", apiDetails)]

apiCompleteJson :: Json
apiCompleteJson = Json $ object
    [ ( "/test_route/{test::()}/{test::()}"
      , object
        [ ( "StreamBody"
          , object [ ( "Format", String "()")
                   , ( "ContentType", String "()")
                   ]
          )
        , ( "Summary", String "sampleText")
        , ( "Basic Authentication"
          , object [ ( "UserData", String "()")
                   , ( "Realm", String "local")
                   ]
          )
        , ( "Captures Http Version", String "True")
        , ( "QueryParam"
          , object [ ( "Param",String "test1")
                   , ( "ContentType",String "Int")
                   ]
          )
        , ( "QueryParam"
          , object [ ( "Param",String "test2")
                   , ( "ContentType",String "Bool")
                   ]
          )
        , ( "Authentication", String "TEST_JWT")
        , ( "Response"
          , object [ ( "Format", String "[()]")
                   , ( "ContentType", String "()")
                   ]
          )
        , ( "RequestType", String "'POST")
        , ( "Vault", String "True")
        , ( "Captures RemoteHost/IP", String "True")
        , ( "RequestHeaders"
          , object [ ( "Name",String "test")
                   , ( "ContentType",String "()")
                   ]
          )
        , ( "SSL Only", String "True")
        , ( "QueryParams"
          , object [ ( "Param",String "test")
                   , ( "ContentType",String "()")
                   ]
          )
        , ( "Description", String "sampleText")
        , ( "QueryFlag", object [( "Param",String "test")])
        , ( "RequestBody"
          , object [ ( "Format", String "[()]")
                   , ( "ContentType",String "()")
                   ]
          )
        ]
      )
    ]

apiCompletePlainText :: PlainText
apiCompletePlainText = PlainText [r|/test_route/{test::()}/{test::()}:
Captures Http Version: True
SSL Only: True
Captures RemoteHost/IP: True
Description: sampleText
Summary: sampleText
Vault: True
Basic Authentication:
    Realm: local
    UserData: ()
Authentication: TEST_JWT
RequestHeaders:
    Name: test
    ContentType: ()
QueryFlag:
    Param: test
QueryParam:
    Param: test1
    ContentType: Int
QueryParam:
    Param: test2
    ContentType: Bool
QueryParams:
    Param: test
    ContentType: ()
RequestBody:
    Format: [()]
    ContentType: ()
StreamBody:
    Format: ()
    ContentType: ()
RequestType: 'POST
Response:
    Format: [()]
    ContentType: ()|]

type ApiMultiple = "route1" :> DynRouteTest :> CaptureAllTest :> ApiDetails
              :<|> "route2" :> DynRouteTest :> CaptureAllTest :> ApiDetails
              :<|> "route3" :> DynRouteTest :> CaptureAllTest :> ApiDetails
              :<|> "get" :> GetTest
              :<|> "post" :> PostTest
              :<|> "put" :> PutTest
              :<|> "patch" :> PatchTest

apiMultipleParsed :: ApiDocs
apiMultipleParsed = ApiDocs 
    [ ("/route1/{test::()}/{test::()}", apiDetails)
    , ("/route2/{test::()}/{test::()}", apiDetails)
    , ("/route3/{test::()}/{test::()}", apiDetails)
    , ("/get", Details [ ("RequestType", Detail "'GET")
                       , ("Response", Details [ ("Format",Detail "[JSON]")
                                              , ("ContentType",Detail "()")
                                              ])
                       ])
    , ("/post", Details [ ("RequestType",Detail "'POST")
                        , ("Response", Details [ ("Format",Detail "[()]")
                                               , ("ContentType",Detail "()")
                                               ])
                       ])
    , ("/put", Details [ ("RequestType",Detail "'PUT")
                       , ("Response", Details [ ("Format",Detail "[()]")
                                              , ("ContentType",Detail "()")
                                              ])
                       ])
    , ("/patch", Details [ ("RequestType", Detail "'PATCH")
                         , ("Response", Details [ ("Format",Detail "[()]")
                                                , ("ContentType",Detail "()")
                                                ])
                         ])
    ]


type ApiDetails = HttpVersionTest
               :> IsSecureTest
               :> RemoteHostTest
               :> DescriptionTest
               :> SummaryTest
               :> VaultTest
               :> BasicAuthTest
               :> AuthTest
               :> HeaderTest
               :> QueryFlagTest
               :> QueryParamTest
               :> QueryParamTest2 -- Check that multiple query parameters can be included in 1 endpoint
               :> QueryParamsTest
               :> ReqBodyTest
               :> StreamBodyTest
               :> PostTest


apiDetails :: Details
apiDetails = Details
    [ ("Captures Http Version", Detail "True")
    , ("SSL Only", Detail "True")
    , ("Captures RemoteHost/IP", Detail "True")
    , ("Description", Detail "sampleText")
    , ("Summary", Detail "sampleText")
    , ("Vault", Detail "True")
    , ("Basic Authentication", Details [ ("Realm", Detail "local")
                                       , ("UserData", Detail "()")
                                       ])
    , ("Authentication", Detail "TEST_JWT")
    , ("RequestHeaders", Details [ ("Name", Detail "test")
                                 , ("ContentType", Detail "()")
                                 ])
    , ("QueryFlag", Details [("Param", Detail "test")])
    , ("QueryParam", Details [ ("Param", Detail "test1")
                             , ("ContentType", Detail "Int")
                             ])
    , ("QueryParam", Details [ ("Param", Detail "test2")
                             , ("ContentType", Detail "Bool")
                             ])
    , ("QueryParams", Details [ ("Param", Detail "test")
                              , ("ContentType", Detail "()")
                              ])
    , ("RequestBody", Details [ ("Format", Detail "[()]")
                              , ("ContentType", Detail "()")
                              ])
    , ("StreamBody", Details [ ("Format", Detail "()")
                             , ("ContentType", Detail "()")
                             ])
    , ("RequestType", Detail "'POST")
    , ("Response", Details [ ("Format", Detail "[()]")
                           , ("ContentType", Detail "()")
                           ])
    ]

type StaticRouteTest = "test_route"

type DynRouteTest = Capture "test" ()

type CaptureAllTest = CaptureAll "test" ()

type HttpVersionTest = HttpVersion

type IsSecureTest = IsSecure

type RemoteHostTest = RemoteHost

type DescriptionTest = Description "sampleText"

type SummaryTest = Summary "sampleText"

type VaultTest = Vault

type BasicAuthTest = BasicAuth "local" ()

type AuthTest = AuthProtect "TEST_JWT"

type HeaderTest = Header "test" ()

type QueryFlagTest = QueryFlag "test"

type QueryParamTest = QueryParam "test1" Int

type QueryParamTest2 = QueryParam "test2" Bool

type QueryParamsTest = QueryParams "test" ()

type ReqBodyTest = ReqBody '[()] ()

type StreamBodyTest = StreamBody () ()

type GetTest = Get '[JSON] ()

type PostTest = Post '[()] ()

type PutTest = Put '[()] ()

type PatchTest = Patch '[()] ()
