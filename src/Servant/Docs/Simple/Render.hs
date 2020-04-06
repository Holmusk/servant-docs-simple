
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

module Servant.Docs.Simple.Render (Endpoints(..), Details(..), Node(..), Renderable(..), Json(..), Pretty(..), PlainText(..)) where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.List (intersperse)
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (Doc, cat, line, nest, pretty, vcat, vsep)

-- | Documentation format which we render to other formats
newtype Endpoints = Endpoints [Node] deriving stock (Eq, Show)

data Details = Details [Node] | Detail Text deriving stock (Eq, Show)
data Node = Node Text Details deriving stock (Eq, Show)

class Renderable a where
  render :: Endpoints -> a

-- | Conversion to JSON using Data.Aeson
newtype Json = Json { getJson :: Value } deriving stock (Eq, Show)
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
prettyPrint (Endpoints ls) = vcat . intersperse line $ toDoc 0 <$> ls

toDoc :: Int -> Node -> Doc ann
toDoc i (Node t d) = case d of
    Detail a   -> cat [pretty t, ": ", pretty a]
    Details as -> nest i . vsep $ pretty t <> ":"
                                : (toDoc (i + 4) <$> as)

-- | Conversion to plaintext
newtype PlainText = PlainText { getPlainText :: Text } deriving stock (Eq, Show)
instance Renderable PlainText where
  render = PlainText . pack . show . getPretty . render
