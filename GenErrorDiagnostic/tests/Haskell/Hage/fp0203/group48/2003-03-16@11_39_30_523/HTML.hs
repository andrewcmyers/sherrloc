module HTML(module HTML, module Pretty) where

import Pretty

data Html   = DubTag String [Atr] [Html]
            | EnkTag String [Atr]
            | Txt String

data Atr    = Atr String String







blah :: Html
blah = DubTag "<UL>" []
              [(DubTag "<Li>" [] [(DubTag "FONT" ["COLOR" "#0000FF"] [Txt "Eerste"]), (Txt "Punt")]),
              (EnkTag "HR" []),
              (DubTag "<Li>" [] [(Txt "Tweede Punt")])]

-- should be: Atr ...
-- 19,51-67
