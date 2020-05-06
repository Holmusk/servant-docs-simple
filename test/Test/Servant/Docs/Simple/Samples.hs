-- | Contains sample Servant API types and their formatted counterparts
{-# LANGUAGE QuasiQuotes #-}

module Test.Servant.Docs.Simple.Samples (ApiComplete, ApiMultiple, apiCompletePlainText,
                                         apiCompleteParsed, apiCompleteJson, apiMultipleParsed) where

import Data.Aeson (Value (String), object)
import Data.Map.Ordered (fromList, singleton)
import Servant.API ((:<|>), (:>), AuthProtect, BasicAuth, Capture, CaptureAll, Description, Header,
                    HttpVersion, IsSecure, Post, QueryFlag, QueryParam, QueryParams, RemoteHost,
                    ReqBody, StreamBody, Summary, Vault)

import Text.RawString.QQ (r)

import Servant.Docs.Simple.Render (Details (..), ApiDocs (..), Json (..), PlainText (..))


type ApiComplete = StaticRouteTest :> DynRouteTest :> CaptureAllTest :> ApiDetails

apiCompleteParsed :: ApiDocs
apiCompleteParsed = ApiDocs $ curry singleton "/test_route/{test::()}/{test::()}" apiDetails

apiCompleteJson :: Json
apiCompleteJson = Json (object [ ( "/test_route/{test::()}/{test::()}"
                                 , object [ ( "StreamBody"
                                            , object [ ( "Format", String "()")
                                                     , ( "ContentType", String "()")
                                                     ])

                                          , ( "Summary",String "sampleText" )

                                          , ( "Basic Authentication"
                                            , object [ ( "UserData", String "()")
                                                     , ( "Realm", String "local")
                                                     ])

                                          , ( "Captures Http Version", String "True")

                                          , ( "QueryParam"
                                            , object [ ( "Param",String "test")
                                                     , ( "ContentType",String "()")
                                                     ])

                                          , ( "Authentication", String "TEST_JWT")

                                          , ( "Response"
                                            , object [ ( "Format", String "': * () ('[] *)")
                                                     , ( "ContentType",String "()")
                                                     ])

                                          , ( "RequestType", String "'POST")

                                          , ( "Vault", String "True")

                                          , ( "Captures RemoteHost/IP", String "True")

                                          , ( "RequestHeaders"
                                            , object [ ( "Name",String "test")
                                                     , ( "ContentType",String "()")
                                                     ])

                                          , ( "SSL Only",String "True")

                                          , ( "QueryParams"
                                            , object [ ( "Param",String "test")
                                                     , ( "ContentType",String "()")
                                                     ])

                                          , ( "Description", String "sampleText")

                                          , ( "QueryFlag", object [( "Param",String "test")])

                                          , ( "RequestBody"
                                            , object [ ( "Format", String "': * () ('[] *)")
                                                     , ( "ContentType",String "()")
                                                     ])

                                          ])])

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
    Param: test
    ContentType: ()
QueryParams:
    Param: test
    ContentType: ()
RequestBody:
    Format: ': * () ('[] *)
    ContentType: ()
StreamBody:
    Format: ()
    ContentType: ()
RequestType: 'POST
Response:
    Format: ': * () ('[] *)
    ContentType: ()|]

type ApiMultiple = "route1" :> DynRouteTest :> CaptureAllTest :> ApiDetails
              :<|> "route2" :> DynRouteTest :> CaptureAllTest :> ApiDetails
              :<|> "route3" :> DynRouteTest :> CaptureAllTest :> ApiDetails

apiMultipleParsed :: ApiDocs
apiMultipleParsed = ApiDocs $ fromList $ [ ("/route1/{test::()}/{test::()}", apiDetails)
                                         , ("/route2/{test::()}/{test::()}", apiDetails)
                                         , ("/route3/{test::()}/{test::()}", apiDetails)
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
               :> QueryParamsTest
               :> ReqBodyTest
               :> StreamBodyTest
               :> ResponseTest

apiDetails :: Details
apiDetails = Details $ fromList [ ("Captures Http Version", Detail "True")
                                , ("SSL Only", Detail "True")
                                , ("Captures RemoteHost/IP", Detail "True")
                                , ("Description", Detail "sampleText")
                                , ("Summary", Detail "sampleText")
                                , ("Vault", Detail "True")
                                , ("Basic Authentication", Details $ fromList [ ("Realm", Detail "local")
                                                                              , ("UserData", Detail "()")
                                                                              ])

                                , ("Authentication", Detail "TEST_JWT")

                                , ("RequestHeaders", Details $ fromList [ ("Name", Detail "test")
                                                                        , ("ContentType", Detail "()")
                                                                        ])

                                , ("QueryFlag", Details $ curry singleton "Param" (Detail "test"))

                                , ("QueryParam", Details $ fromList [ ("Param", Detail "test")
                                                                    , ("ContentType", Detail "()")
                                                                    ])

                                , ("QueryParams", Details $ fromList [ ("Param", Detail "test")
                                                                     , ("ContentType", Detail "()")
                                                                     ])

                                , ("RequestBody", Details $ fromList [ ("Format", Detail "': * () ('[] *)")
                                                                     , ("ContentType", Detail "()")
                                                                     ])

                                , ("StreamBody", Details $ fromList [ ("Format", Detail "()")
                                                                    , ("ContentType", Detail "()")
                                                                    ])

                                , ("RequestType", Detail "'POST")

                                , ("Response", Details $ fromList [ ("Format", Detail "': * () ('[] *)")
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

type QueryParamTest = QueryParam "test" ()

type QueryParamsTest = QueryParams "test" ()

type ReqBodyTest = ReqBody '[()] ()

type StreamBodyTest = StreamBody () ()

type ResponseTest = Post '[()] ()
