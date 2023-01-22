import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as Bl
import Data.Foldable ()
import System.Process ( runCommand )
import Text.Printf ( printf )

type Wave = [Float]

volume :: Float
volume = 0.5

outPath :: FilePath
outPath = "output.bin"

sampleRate :: Float
sampleRate = 48000

step :: Float
step = 0.03

soundPrep :: Wave -> Wave
soundPrep = map ((* volume) . sin . (* step)) 

wave :: [Float]
wave = soundPrep [0.0 .. sampleRate]

save :: FilePath ->  IO()    
save filePath = B.writeFile filePath $ Bl.toLazyByteString $ foldMap Bl.floatLE wave 

play :: IO()
play = do
    save outPath
    _ <- runCommand $ printf "ffplay -f f32le -ar  %f %s" sampleRate outPath
    return ()