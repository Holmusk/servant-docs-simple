-- | Unit tests for generating documentation of api

module Test.Servant.Docs.Simple (mainSpec) where


import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Servant.Docs.Simple.Samples

import Servant.Docs.Simple (document, documentWith)
import Servant.Docs.Simple.Render (Json)


mainSpec :: Spec
mainSpec = describe "Generates Documentation" $ do
    it "should generate Plaintext" $
        document @ApiComplete `shouldBe` apiCompletePlainText
    it "should generate JSON" $
        documentWith @ApiComplete @Json `shouldBe` apiCompleteJson
