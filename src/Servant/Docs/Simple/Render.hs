-- | Provides an Intermediate documentation structure ('Endpoints'), and renderable formats ('Renderable')
module Servant.Docs.Simple.Render ( Details (..)
                                  , Endpoints (..)
                                  , Node (..)
                                  , Renderable (..)
                                  , Json (..)
                                  , Pretty (..)
                                  , PlainText (..)
                                  ) where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.List (intersperse)
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (Doc, cat, line, nest, pretty, vcat, vsep)

-- | Intermediate documentation structure, a linked-list of endpoints (__'Node's__)
--
-- API type:
--
-- >   type API = "users" :> (      "update" :> Response '[()] ()
-- >                           :<|> "get"    :> Response '[()] ()
-- >                         )
--
-- Parsed into Endpoints:
--
-- >   Endpoints [ Node "/users/update"
-- >                    (Details [ Node "Response"
-- >                                    (Details [ Node "Format" (Detail "': * () ('[] *)")
-- >                                             , Node "ContentType" (Detail "()")
-- >                                             ])
-- >                             ])
-- >             , Node "/users/get"
-- >                    (Details [ Node "Response"
-- >                                    (Details [ Node "Format" (Detail "': * () ('[] *)")
-- >                                             , Node "ContentType" (Detail "()")
-- >                                             ])
-- >                             ])
-- >             ]
--
-- For a breakdown reference 'Node'
--
-- For more examples reference [Test.Servant.Docs.Simple.Samples](https://github.com/Holmusk/servant-docs-simple/blob/master/test/Test/Servant/Docs/Simple/Samples.hs)
--
newtype Endpoints = Endpoints [Node] deriving stock (Eq, Show)

-- | Key-Value pair for endpoint parameters and their values
--
-- __Example 1__
--
-- An endpoint is represented as a node, with the __route__ as its parameter and its Details as its value
--
-- > Node "/users/get" <Details>
--
-- __Example 2__
--
-- Details of each endpoint can also be represented as nodes
--
-- Given the following:
--
-- > Response '[()] ()
--
-- This can be interpreted as a __Response__ parameter, with a value of 2 Details, __Format__ and __ContentType__
--
-- In turn, this:
--
-- > Format: @'[()]@
--
-- can be interpreted as a __Format__ parameter with a value of __@'[()]@__.
--
-- And so parsing __@Response '[()] ()@__ comes together as:
--
-- > Node "Response"                                               --- Parameter
-- >      (Details [ Node "Format"                   -- Parameter  --- 
-- >                      (Detail "': * () ('[] *)") -- Value         |
-- >               , Node "ContentType"              -- Parameter     | Value
-- >                      (Detail "()")              -- Value         |
-- >               ])                                              ---
--

data Node = Node Text -- ^ Parameter name
                 Details -- ^ Parameter value(s)
            deriving stock (Eq, Show)

-- | Value representation; see 'Endpoints' and 'Node' documentation for a clearer picture
data Details = Details [Node] -- ^ List of Parameter-Value pairs
             | Detail Text    -- ^ Single Value
             deriving stock (Eq, Show)

-- | Convert __Endpoints__ into different documentation formats
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
