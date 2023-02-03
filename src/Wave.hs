module Wave 
(squareWave)
where

import Lib ()

squareWave :: Float -> Float
squareWave t = t * v2 + (-(t - 1)) * v1
    where
        v1 
            |t>=0.5 = 1
            |otherwise = 0
        v2 
            |t < 0.5 = 1
            |otherwise =0
