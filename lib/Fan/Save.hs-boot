module Fan.Save
    ( loadHead
    , loadBody
    , saveFan
    , saveFan'
    , subPins
    )
where

import Fan.Types      (Fan, Pin)
import Jelly.Fast.FFI (Ctx)
import Jelly.Types    (Hash256)
import PlunderPrelude (ByteString, Either, IO, Text, Vector)

--------------------------------------------------------------------------------

loadHead :: ByteString -> Either Text (Vector Hash256)

loadBody :: Vector Pin -> ByteString -> Either Text Fan

saveFan :: Fan -> IO (Vector Pin, ByteString, ByteString)

saveFan' :: Ctx -> Fan -> IO (Vector Pin, ByteString, ByteString)

subPins :: Fan -> [Pin]
