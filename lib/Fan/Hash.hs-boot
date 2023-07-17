module Fan.Hash (fanHash) where

import Fan.Types      (Fan)
import PlunderPrelude (ByteString)
import Jelly.Types    (Hash256)

--------------------------------------------------------------------------------

fanHash :: Fan -> Hash256
