-- | Unit tests for generating documentation of api

module Test.Servant.Docs.Simple (docSpec) where

import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Text (Text, pack, stripSuffix)
import Servant.API ((:<|>), (:>), AuthProtect, Capture, Header, Post, QueryParam, ReqBody)
import Servant.API.TypeLevel (Endpoints)
import Servant.Docs.Simple (collate, documentEndpoint)
import Test.Hspec (Spec, describe, it, shouldBe)

docSpec :: Spec
docSpec = describe "Generating Documentation" $ do
    it "Generates static route fragment" $
        stripResponse (documentEndpoint @(StaticRouteTest :> ResponseTest))
        `shouldBe` Just staticRouteExpected
    it "Generates dynamic route doc fragment" $
        stripResponse (documentEndpoint @(DynRouteTest :> ResponseTest))
        `shouldBe` Just dynRouteExpected
    it "Generates Authentication doc fragment" $
        stripResponse (documentEndpoint @(AuthTest :> ResponseTest))
        `shouldBe` Just authExpected
    it "Generates Header fragment" $
        stripResponse (documentEndpoint @(HeaderTest :> ResponseTest))
        `shouldBe` Just headerExpected
    it "Generates Query param fragment" $
        stripResponse (documentEndpoint @(QueryParamTest :> ResponseTest))
        `shouldBe` Just queryParamExpected
    it "Generates Request body fragment" $
        stripResponse (documentEndpoint @(ReqBodyTest :> ResponseTest))
        `shouldBe` Just reqBodyExpected
    it "Generates Response fragment" $
        responseSuffix
        `shouldBe` responseExpected
    it "Documents entire api route" $
        (pack . show) (documentEndpoint @ApiRoute)
        `shouldBe` apiRouteExpected
    it "Collates api routes" $
        (pack . show) (collate @(Endpoints (ApiRoute :<|> ApiRoute)))
        `shouldBe` apiCollatedExpected
    where stripResponse = stripSuffix ("\n" <> responseSuffix) . pack . show

type StaticRouteTest = "test_route"
staticRouteExpected :: Text
staticRouteExpected = "/test_route"

type DynRouteTest = Capture "test" ()
dynRouteExpected :: Text
dynRouteExpected = "/{test::()}"

type AuthTest = AuthProtect "TEST_JWT"
authExpected :: Text
authExpected = "Authentication: TEST_JWT"

type HeaderTest = Header "test" ()
headerExpected :: Text
headerExpected = vcat [ "RequestHeaders:"
                      , "  Name: test"
                      , "  ContentType: ()"
                      ]

type QueryParamTest = QueryParam "test" ()
queryParamExpected :: Text
queryParamExpected = vcat [ "QueryParam:"
                          , "  Param: test"
                          , "  ContentType: ()"
                          ]

type ReqBodyTest = ReqBody '[()] ()
reqBodyExpected :: Text
reqBodyExpected = vcat [ "RequestBody:"
                       , "  Format: ': * () ('[] *)"
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
responseSuffix = pack $ show $ documentEndpoint @ResponseTest

vcat :: [Text] -> Text
vcat = fold . intersperse ("\n" :: Text)
