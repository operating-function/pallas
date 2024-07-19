module Fan.Trace where

import PlunderPrelude
import Fan

import qualified Fan.Prof as Prof

--------------------------------------------------------------------------------

doTrk :: Fan -> a -> a
doTrk msg val = unsafePerformIO do
    trk <- readIORef vTrkFan
    tag <- evaluate (force msg)
    trk msg

    case trkName tag of
        Nothing -> evaluate val
        Just (encodeUtf8 -> nm) ->
            Prof.withAlwaysTrace nm "trk" do
                evaluate val

