module Servant.Docs.Simple (document, documentWith) where

import Servant.Docs.Simple.Render
import Servant.Docs.Simple.Parse

document :: forall api. HasParsable api => PlainText
document = documentWith @api @PlainText

documentWith :: forall api a. (HasParsable api, Renderable a) => a
documentWith = render @a (parse @api)
