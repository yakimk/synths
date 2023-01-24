import qualified Data.ByteString.Builder as Bl
import qualified Data.ByteString.Lazy    as B
import           Data.Foldable           ()
import           Data.List
import           Pitch                   hiding (Hz)
import           System.Process          (runCommand)
import           Text.Printf             (printf)

type Wave = [Float]
type Volume = Float
type Sec = Float
type Samples = Float
type Hz = Float
type Note = (Hz, Beat, Volume)
type Phrase = [Note]
type Beat = Float

bpm :: Beat
bpm = 100

beatDuration :: Sec
beatDuration = 60/bpm

comb :: Fractional a => [a] -> [a] -> [a]
comb xs [] = xs
comb [] ys = ys
comb (x:xs) (y:ys) = res : comb xs ys
    where
        res = (x+y) / 2.0

volume :: Float
volume = 0.3

outPath :: FilePath
outPath = "output.bin"

sampleRate :: Samples
sampleRate = 48000

note :: Note -> Wave
note (freq, b) = zipWith3 (\x y z -> x*y*z) volumes release output
    where
        note = (2*freq*pi)/sampleRate
        volumes = map  (* volume) attack
        attack = map (min 1.0) [0.0, 0.001 ..]
        release = reverse $ take (length output) attack
        output =  map (sin . (* note)) [0.0 .. (b*beatDuration) * sampleRate]

phrase :: Phrase -> Wave
phrase = concatMap note

wave :: Phrase -> Wave
wave  = phrase

waves :: (Wave, Wave) -> Wave
waves (x,y) = comb x y

waves' :: [Wave] -> Wave
waves' []       = []
waves' [x]      = x
waves' (x:xs) = comb x $  waves' xs

save :: FilePath ->  IO()
save filePath = B.writeFile filePath $ Bl.toLazyByteString $ foldMap Bl.floatLE $ waves' [wave line1, wave line2, wave line3, wave line4]

play :: IO()
play = do
    save outPath
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar  %f %s" sampleRate outPath
    return ()


line1 :: Phrase
line1 = [(a4, 1), (g6, 1), (a4, 1), (a3,2)]

line2 :: Phrase
line2= [(b4,1), (c3,2), (e2, 2)]

line3 :: Phrase
line3= [(b2,1), (c2,2), (e1, 2)]

line4 :: Phrase
line4= [(e5,1), (b4,2), (ds6, 2)]