module Main (main) where

import Lib

import qualified Data.ByteString.Builder as Bl
import qualified Data.ByteString.Lazy    as B
import           Data.Foldable           ()
import qualified Data.List()
import           Pitch                   hiding (Hz)
import           System.Process          (runCommand)
import           Text.Printf             (printf)
import qualified VolumeModulator as VM

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

volume :: Volume
volume = 0.3

outPath :: FilePath
outPath = "output.bin"

sampleRate :: Samples
sampleRate = 48000

note :: Note -> Wave
note [freq, beats, vol] = zipWith3 (\x y z -> x*y*z) volumes release output
    where
        pitch = (2*freq*pi)/sampleRate
        volumes = map  (* vol) attack
        attack = map (min 1.0) [0.0, 0.001 ..]
        release = reverse $ take (length output) attack
        output =  map (sin . (* pitch)) [0.0 .. (beats*beatDuration) * sampleRate]
note x = x

phrase :: Phrase -> Wave
phrase = concatMap note . parsePhrase

wave :: Phrase -> Wave
wave  = phrase

waves :: [Wave] -> Wave
waves []       = []
waves [x]      = x
waves (x:xs) = comb x $  waves xs

volumeNormalization :: Wave -> Wave
volumeNormalization = map (*volume)

save :: FilePath ->  IO()
save filePath = 
    B.writeFile filePath 
    $ Bl.toLazyByteString 
    $ foldMap Bl.floatLE 
    -- $ VM.sinVolumeModulation 8
    $ VM.squareVolumeModulation 40
    $ volumeNormalization 
    $ waves [wave line1, wave line2, wave line3, wave line4]

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
parsePhrase = map parseNote

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
