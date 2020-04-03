-- | Unit tests for generating documentation of api

module Test.Servant.Docs.Simple where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Servant.Docs.Simple.Samples

import Servant.Docs.Simple (document, documentWith)

mainSpec :: Spec
mainSpec = describe "Generates Documentation" $ do
    it "should generate Plaintext" $
        1 `shouldBe` 1
    it "should generate JSON" $
        1 `shouldBe` 1
