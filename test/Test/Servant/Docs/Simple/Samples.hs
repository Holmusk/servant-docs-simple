-- | Contains sample Servant API types and their formatted counterparts

module Test.Servant.Docs.Simple.Samples where

import Data.Text (Text, pack, stripSuffix)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Foldable (fold)
import Data.List (intersperse)
import Servant.API ((:>), AuthProtect, BasicAuth, Capture, CaptureAll,
                    Description, Header, HttpVersion, IsSecure, Post, QueryFlag,
                    QueryParam, QueryParams, RemoteHost, ReqBody,
                    StreamBody, Summary, Vault)

type StaticRouteTest = "test_route"
staticRouteExpected :: Text
staticRouteExpected = "/test_route"

type DynRouteTest = Capture "test" ()
dynRouteExpected :: Text
dynRouteExpected = "/{test::()}"

type CaptureAllTest = CaptureAll "test" ()
captureAllExpected :: Text
captureAllExpected = "/{test::()}"

type HttpVersionTest = HttpVersion
httpVersionExpected :: Text
httpVersionExpected = "Captures Http Version: True"

type IsSecureTest = IsSecure
isSecureExpected :: Text
isSecureExpected = "SSL Only: True"

type RemoteHostTest = RemoteHost
remoteHostExpected :: Text
remoteHostExpected = "Captures RemoteHost/IP: True"

type DescriptionTest = Description "sampleText"
descriptionExpected :: Text
descriptionExpected = "Description: sampleText"

type SummaryTest = Summary "sampleText"
summaryExpected :: Text
summaryExpected = "Summary: sampleText"

type VaultTest = Vault
vaultExpected :: Text
vaultExpected = "Vault: True"

type BasicAuthTest = BasicAuth "local" ()
basicAuthExpected :: Text
basicAuthExpected = vcat [ "Basic Authentication: "
                         , "  Realm: local"
                         , "  UserData: ()"
                         ]

type AuthTest = AuthProtect "TEST_JWT"
authExpected :: Text
authExpected = "Authentication: TEST_JWT"

type HeaderTest = Header "test" ()
headerExpected :: Text
headerExpected = vcat [ "RequestHeaders:"
                      , "  Name: test"
                      , "  ContentType: ()"
                      ]

type QueryFlagTest = QueryFlag "test"
queryFlagExpected :: Text
queryFlagExpected = vcat [ "QueryFlag:"
                         , "  Param: test"
                         ]

type QueryParamTest = QueryParam "test" ()
queryParamExpected :: Text
queryParamExpected = vcat [ "QueryParam:"
                          , "  Param: test"
                          , "  ContentType: ()"
                          ]

type QueryParamsTest = QueryParams "test" ()
queryParamsExpected :: Text
queryParamsExpected = vcat [ "QueryParams:"
                          , "  Param: test"
                          , "  ContentType: ()"
                          ]

type ReqBodyTest = ReqBody '[()] ()
reqBodyExpected :: Text
reqBodyExpected = vcat [ "RequestBody:"
                       , "  Format: ': * () ('[] *)"
                       , "  ContentType: ()"
                       ]

type StreamBodyTest = StreamBody () ()
streamBodyExpected :: Text
streamBodyExpected = vcat [ "StreamBody:"
                          , "  Format: ()"
                          , "  ContentType: ()"
                          ]

type ResponseTest = Post '[()] ()
responseExpected :: Text
responseExpected = vcat [ "RequestType: 'POST"
                        , "Response:"
                        , "  Format: ': * () ('[] *)"
                        , "  ContentType: ()"
                        ]

type ApiRoute = StaticRouteTest
             :> AuthTest
             :> DynRouteTest
             :> HeaderTest
             :> ReqBodyTest
             :> ResponseTest

apiRouteExpected :: Text
apiRouteExpected = vcat [ staticRouteExpected <> dynRouteExpected
                        , authExpected
                        , headerExpected
                        , reqBodyExpected
                        , responseExpected
                        ]

apiCollatedExpected :: Text
apiCollatedExpected = vcat [ apiRouteExpected <> "\n\n"
                           , apiRouteExpected <> "\n\n"
                           ]

-- Helpers --
responseSuffix :: Text
responseSuffix = responseExpected

vcat :: [Text] -> Text
vcat = fold . intersperse ("\n" :: Text)

stripResponse :: Doc ann -> Maybe Text
stripResponse = stripSuffix ("\n" <> responseSuffix) . pack . show
