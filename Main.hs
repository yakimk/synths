import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as Bl
import Data.Foldable ()
import System.Process ( runCommand )
import Text.Printf ( printf )

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
step = 0.03

soundPrep :: Hz -> Wave -> Wave
soundPrep freq = map ((* volume) . sin . (* (step * note)))
    where
        note = 1/freq

wave :: [Float]
wave = soundPrep 440 [0.0 .. duration * sampleRate]
    where 
        duration :: Sec
        duration = 0.5

save :: FilePath ->  IO()    
save filePath = B.writeFile filePath $ Bl.toLazyByteString $ foldMap Bl.floatLE wave 

play :: IO()
play = do
    save outPath
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar  %f %s" sampleRate outPath
    return ()