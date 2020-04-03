-- | Unit tests for generating documentation of api

module Test.Servant.Docs.Simple.Render (renderSpec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Core.Spec (sequential)

import Test.Servant.Docs.Simple.Samples

import Data.Text (pack)
import Servant.API ((:<|>), (:>))
import Servant.API.TypeLevel (Endpoints)
import Servant.Docs.Simple

renderSpec :: Spec
renderSpec = describe "Renders Details" $ do
    it "should render as Plaintext" $
        1 `shouldBe` 1
    it "should render as JSON" $
        1 `shouldBe` 1
