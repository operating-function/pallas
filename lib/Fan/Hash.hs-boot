module Fan.Hash (fanHash) where

import Fan.Types      (Fan)
import PlunderPrelude (ByteString)
import Hash256        (Hash256)

--------------------------------------------------------------------------------

fanHash :: Fan -> Hash256
