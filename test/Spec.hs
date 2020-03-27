import Test.Hspec (hspec)
import Test.Servant.Docs.Simple (collateSpec, docSpec)

main :: IO ()
main = do
  hspec docSpec
  hspec collateSpec
