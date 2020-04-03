
{-| Simple documentation for API endpoints

TODO: Add language extensions used

Example:

@
    import Servant.Docs.Simple (collate)
    import Servant.API.Typelevel (Endpoints, ReqBody', Request)

    type Request = ReqBody '[()] ()
    type Response = Post '[()] ()
    type API = "hello" :> "world" :> Request :> Response

    documentation :: Text
    documentation = collate @(Endpoints API)
@

--}

module Servant.Docs.Simple.Render (Endpoints(..), Details(..), Node(..), Renderable(..), Json(..), PlainText(..)) where

import Data.Aeson (ToJSON (..), Value (..), (.=), object)
import Data.Text (Text, pack)
import Data.List (intersperse)
import Data.Text.Prettyprint.Doc (Doc, pretty, line, nest, vcat, cat)

-- | Documentation format which we render to other formats
data Endpoints = Endpoints [Node]

data Details = Details [Node] | Detail Text
data Node = Node Text Details

class Renderable a where
  render :: Endpoints -> a

-- | Conversion to JSON using Data.Aeson
newtype Json = Json { getJson :: Value }
instance Renderable Json where
  render = Json . toJSON

instance ToJSON Endpoints where
    toJSON (Endpoints endpoints) = toJSON $ Details endpoints

instance ToJSON Details where
    toJSON (Detail t) = String t
    toJSON (Details ls) = object . fmap jsonify $ ls
      where jsonify (Node name details) = name .= toJSON details

-- | Conversion to prettyprint
newtype Pretty ann = Pretty { getPretty :: Doc ann }

instance Renderable (Pretty ann) where
  render = Pretty . prettyPrint

prettyPrint :: Endpoints -> Doc ann
prettyPrint (Endpoints ls) = vcat . intersperse line $ fmap (toDoc 0) $ ls

toDoc :: Int -> Node -> Doc ann
toDoc i (Node t d) = nest i formatted
  where formatted = parameter <> body
        parameter = pretty t <> ":"
        body = case d of
            Detail a -> " " <> pretty a
            Details as -> cat $ toDoc (i + 2) <$> as

-- | Conversion to plaintext
newtype PlainText = PlainText Text
instance Renderable PlainText where
  render = PlainText . pack . show . getPretty . render
