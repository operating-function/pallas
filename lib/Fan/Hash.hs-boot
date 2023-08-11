module Fan.Hash (fanHash) where

import Fan.Types      (Fan)
import Hash256        (Hash256)
import PlunderPrelude (ByteString)

--------------------------------------------------------------------------------

fanHash :: Fan -> Hash256
