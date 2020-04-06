import Test.Hspec
import Test.Hspec.Core.Spec (sequential)

import Servant.Docs.Simple (writeDocsJson, writeDocsPlainText)
import Test.Servant.Docs.Simple (mainSpec)
import Test.Servant.Docs.Simple.Parse (parseSpec)
import Test.Servant.Docs.Simple.Render (renderSpec)
import Test.Servant.Docs.Simple.Samples (ApiMultiple)

main :: IO ()
main = do
  writeDocsPlainText @ApiMultiple "documentation"
  writeDocsJson @ApiMultiple "documentationJson"

  hspec $ sequential $ do
    renderSpec
    parseSpec
    mainSpec
