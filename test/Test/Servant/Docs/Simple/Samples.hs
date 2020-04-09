-- | Contains sample Servant API types and their formatted counterparts
{-# LANGUAGE QuasiQuotes #-}

module Test.Servant.Docs.Simple.Samples (ApiComplete, ApiMultiple, apiCompletePlainText,
                                         apiCompleteParsed, apiCompleteJson, apiMultipleParsed) where

import Data.Aeson (Value (String), object)
import Servant.API ((:<|>), (:>), AuthProtect, BasicAuth, Capture, CaptureAll, Description, Header,
                    HttpVersion, IsSecure, Post, QueryFlag, QueryParam, QueryParams, RemoteHost,
                    ReqBody, StreamBody, Summary, Vault)

import Text.RawString.QQ (r)

import Servant.Docs.Simple.Render (Details (..), Endpoints (..), Json (..), Node (..),
                                   PlainText (..))

type ApiComplete = StaticRouteTest
                  :> DynRouteTest
                  :> CaptureAllTest
                  :> HttpVersionTest
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

apiCompleteParsed :: Endpoints
apiCompleteParsed = Endpoints [ Node "/test_route/{test::()}/{test::()}"
                              (Details [ Node "Captures Http Version" (Detail "True")
                                       , Node "SSL Only" (Detail "True")
                                       , Node "Captures RemoteHost/IP" (Detail "True")
                                       , Node "Description" (Detail "sampleText")
                                       , Node "Summary" (Detail "sampleText")
                                       , Node "Vault" (Detail "True")
                                       , Node "Basic Authentication" (Details [ Node "Realm" (Detail "local")
                                                                              , Node "UserData" (Detail "()")])

                                       , Node "Authentication" (Detail "TEST_JWT")

                                       , Node "RequestHeaders" (Details [ Node "Name" (Detail "test")
                                                                        , Node "ContentType" (Detail "()")])

                                       , Node "QueryFlag" (Details [Node "Param" (Detail "test")])

                                       , Node "QueryParam" (Details [ Node "Param" (Detail "test")
                                                                    , Node "ContentType" (Detail "()")])

                                       , Node "QueryParams" (Details [ Node "Param" (Detail "test")
                                                                     , Node "ContentType" (Detail "()")])

                                       , Node "RequestBody" (Details [ Node "Format" (Detail "': * () ('[] *)")
                                                                     , Node "ContentType" (Detail "()")])

                                       , Node "StreamBody" (Details [ Node "Format" (Detail "()")
                                                                    , Node "ContentType" (Detail "()")])

                                       , Node "RequestType" (Detail "'POST")

                                       , Node "Response" (Details [ Node "Format" (Detail "': * () ('[] *)")
                                                                  , Node "ContentType" (Detail "()")])])]

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

type ApiMultiple = ApiComplete :<|> ApiComplete :<|> ApiComplete

apiMultipleParsed :: Endpoints
apiMultipleParsed = Endpoints $
                    apiCompleteParsed'
                 <> apiCompleteParsed'
                 <> apiCompleteParsed'
                 where Endpoints apiCompleteParsed' = apiCompleteParsed

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
