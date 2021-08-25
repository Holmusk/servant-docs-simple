-- | Unit tests for generating documentation of api

module Test.Servant.Docs.Simple.Render (renderSpec) where


import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Servant.Docs.Simple.Samples (apiCompleteJson, apiCompleteParsed, apiCompletePlainText)

import Servant.Docs.Simple.Render (Json (..), PlainText (..), render)


renderSpec :: Spec
renderSpec = describe "render" $ do
    it "should render as JSON" $
        render @Json apiCompleteParsed `shouldBe` apiCompleteJson
    it "should render as PlainText" $
        render @PlainText apiCompleteParsed `shouldBe` apiCompletePlainText
