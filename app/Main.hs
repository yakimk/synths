module Main (main) where

import Lib ( Beat, Note, Phrase, Samples, Sec, Volume, Wave, WaveFunction, WaveTransform)

import qualified Wave
import qualified Data.ByteString.Builder as Bl
import qualified Data.ByteString.Lazy    as B
import           Data.Foldable           ()
import qualified Data.List()
import Pitch ( a3, a4, b2, b4, c2, c3, ds6, e1, e2, e5, g6 )
import           System.Process          (runCommand)
import           Text.Printf             (printf)
import qualified Oscillation as VM

bpm :: Beat
bpm = 100

beatDuration :: Sec
beatDuration = 60/bpm

defaultWavetransform :: WaveTransform
defaultWavetransform = sin

comb :: Fractional a => [a] -> [a] -> [a]
comb xs [] = xs
comb [] ys = ys
comb (x:xs) (y:ys) = res : comb xs ys
    where
        res = (x+y) / 2.0

volume :: Volume
volume = 0.3

outPath :: FilePath
outPath = "output.bin"

sampleRate :: Samples
sampleRate = 48000

note :: Maybe WaveTransform -> Note -> Wave
note Nothing n = note (Just defaultWavetransform) n
note (Just a) [freq, beats, vol] = map (*vol) output
    where
        pitch = (2*freq*pi)/sampleRate
        output =  map (a . (*pitch)) [0.0 .. (beats*beatDuration) * sampleRate]

compression :: WaveFunction
compression output = zipWith (*) comp output
    where
        comp = zipWith (*) attack release
        attack = map (min 1.0) [0.0, 0.001 ..]
        release = reverse $ take (length output) attack

phrase :: Phrase -> Wave
phrase = concatMap (compression . note (Just Wave.squareWave)) . parsePhrase

wave :: Phrase -> Wave
wave  = phrase

waves :: [Wave] -> Wave
waves []       = []
waves [x]      = x
waves (x:xs) = comb x $  waves xs

volumeNormalization :: WaveFunction
volumeNormalization = map (*volume)

save :: FilePath ->  IO()
save filePath = 
    B.writeFile filePath 
    $ Bl.toLazyByteString 
    $ foldMap Bl.floatLE 
    -- $ VM.sinVolumeModulation 8
    -- $ VM.squareVolumeModulation 40
    -- $ VM.squareOscillator1 1
    $ volumeNormalization 
    $ wave line1
    -- $ waves [wave line1, wave line2, wave line3, wave line4]

play :: IO()
play = do
    save outPath
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar  %f %s" sampleRate outPath
    return ()

parseNote :: Fractional a => [a] -> [a]
parseNote [] = []
parseNote [x] = [x,1,1]
parseNote [x,y] = [x,y,1]
parseNote [x,y,z] = [x,y,z]
parseNote x = x

parsePhrase :: Phrase -> Phrase
parsePhrase =  map parseNote

line1 :: Phrase
line1 = [[a4, 1], [g6, 1], [a4, 1], [a3,2]]

line2 :: Phrase
line2= [[b4,1], [c3,2], [e2, 2]]

line3 :: Phrase
line3= [[b2, 1], [c2,2], [e1, 2]]

line4 :: Phrase
line4= [[e5,1], [b4,2], [ds6, 2, 0.5]]

main :: IO ()
main = play
