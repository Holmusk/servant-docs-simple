module Servant.Docs.Simple (document, documentWith) where

import Servant.Docs.Simple.Render
import Servant.Docs.Simple.Parse

document :: forall api. HasCollatable api => PlainText
document = documentWith @api @PlainText

documentWith :: forall api a. (HasCollatable api, Renderable a) => a
documentWith = render @a (collate @api)
