-- | Unit tests for generating documentation of api

module Test.Servant.Docs.Simple (docSpec, collateSpec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Servant.Docs.Simple.Samples

import Data.Text (pack)
import Servant.API ((:<|>), (:>))
import Servant.API.TypeLevel (Endpoints)
import Servant.Docs.Simple (collate, documentEndpoint)

docSpec :: Spec
docSpec = describe "Generates API Documentation for a single endpoint" $ do
    it "Generates static route fragment" $
        stripResponse (documentEndpoint @(StaticRouteTest :> ResponseTest))
        `shouldBe` Just staticRouteExpected

    it "Generates dynamic route doc fragment" $
        stripResponse (documentEndpoint @(DynRouteTest :> ResponseTest))
        `shouldBe` Just dynRouteExpected

    it "Generates HttpVersion fragment" $
       stripResponse (documentEndpoint @(HttpVersionTest :> ResponseTest))
       `shouldBe` Just httpVersionExpected

    it "Generates IsSecure fragment" $
        stripResponse (documentEndpoint @(IsSecureTest :> ResponseTest))
        `shouldBe` Just isSecureExpected

    it "Generates RemoteHost fragment" $
        stripResponse (documentEndpoint @(RemoteHostTest :> ResponseTest))
        `shouldBe` Just remoteHostExpected

    it "Generates Description fragment" $
        stripResponse (documentEndpoint @(DescriptionTest :> ResponseTest))
        `shouldBe` Just descriptionExpected

    it "Generates Summary fragment" $
        stripResponse (documentEndpoint @(SummaryTest :> ResponseTest))
        `shouldBe` Just summaryExpected

    it "Generates Vault fragment" $
        stripResponse (documentEndpoint @(VaultTest :> ResponseTest))
        `shouldBe` Just vaultExpected

    it "Generates BasicAuth fragment" $
        stripResponse (documentEndpoint @(BasicAuthTest :> ResponseTest))
        `shouldBe` Just basicAuthExpected

    it "Generates Authentication doc fragment" $
        stripResponse (documentEndpoint @(AuthTest :> ResponseTest))
        `shouldBe` Just authExpected

    it "Generates Header fragment" $
        stripResponse (documentEndpoint @(HeaderTest :> ResponseTest))
        `shouldBe` Just headerExpected

    it "Generates QueryFlag fragment" $
        stripResponse (documentEndpoint @(QueryFlagTest :> ResponseTest))
        `shouldBe` Just queryFlagExpected

    it "Generates Query param fragment" $
        stripResponse (documentEndpoint @(QueryParamTest :> ResponseTest))
        `shouldBe` Just queryParamExpected

    it "Generates QueryParams fragment" $
        stripResponse (documentEndpoint @(QueryParamsTest :> ResponseTest))
        `shouldBe` Just queryParamsExpected

    it "Generates Request body fragment" $
        stripResponse (documentEndpoint @(ReqBodyTest :> ResponseTest))
        `shouldBe` Just reqBodyExpected

    it "Generates StreamBody fragment" $
        stripResponse (documentEndpoint @(StreamBodyTest :> ResponseTest))
        `shouldBe` Just streamBodyExpected

    it "Generates Response fragment" $
        responseSuffix
        `shouldBe` responseExpected

    it "Documents entire api route" $
        (pack . show) (documentEndpoint @ApiRoute)
        `shouldBe` apiRouteExpected

collateSpec :: Spec
collateSpec = describe "Collates API documentation" $ do
    it "Collates api routes" $
        (pack . show) (collate @(Endpoints (ApiRoute :<|> ApiRoute)))
        `shouldBe` apiCollatedExpected

