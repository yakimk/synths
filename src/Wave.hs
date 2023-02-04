module Wave
(squareWave)
 where

import Lib (WaveTransform)

clamp :: Float -> Float
clamp num = max 1 $ min (-1) num

squareWave :: WaveTransform
squareWave t = clamp (st * v2 + (-(st - 1)) * v1)
    where
        st = t/10
        v1 
            |st>=0.5 = 1
            |otherwise = 0
        v2 
            |st < 0.5 = 1
            |otherwise =0
