import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as Bl
import Data.Foldable ()
import System.Process ( runCommand )
import Text.Printf ( printf )
import Pitch hiding (Hz)

type Wave = [Float]
type Sec = Float
type Samples = Float
type Hz = Float

volume :: Float
volume = 0.3

outPath :: FilePath
outPath = "output.bin"

sampleRate :: Samples
sampleRate = 48000

step :: Float
step = (2*a4*pi)/sampleRate

note :: Hz -> Sec -> Wave
note freq s = map ((* volume) . sin . (* note)) [0.0 .. s * sampleRate]
    where
        note = (2*freq*pi)/sampleRate

wave :: Wave
wave =  note cs4 2
save :: FilePath ->  IO()    
save filePath = B.writeFile filePath $ Bl.toLazyByteString $ foldMap Bl.floatLE wave 

play :: IO()
play = do
    save outPath
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar  %f %s" sampleRate outPath
    return ()