import qualified Data.ByteString as B
import Data.ByteString.Builder as B ( toLazyByteString, floatLE )
import Data.Foldable ()

volume :: Float
volume = 0.5

type Wave = [Float]

step :: Float
step = 0.01

soundPrep :: Wave -> Wave
soundPrep = map ((* volume) . sin . (* step)) 

wave :: [Float]
wave = soundPrep [0.0 .. 48000]

save :: IO()    
save = B.writeFile "output.bin" $ B.toLazyByteString $ foldMap B.floatLE wave 