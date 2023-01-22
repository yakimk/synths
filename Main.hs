import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as Bl
import Data.Foldable ()

type Wave = [Float]

volume :: Float
volume = 0.5


step :: Float
step = 0.01

soundPrep :: Wave -> Wave
soundPrep = map ((* volume) . sin . (* step)) 

wave :: [Float]
wave = soundPrep [0.0 .. 48000]

save :: IO()    
save = B.writeFile "output.bin" $ Bl.toLazyByteString $ foldMap Bl.floatLE wave 