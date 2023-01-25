module VolumeModulator(sinVolumeModulation, squareVolumeModulation, squareVolumeModulation1)
 where

import Lib (Wave, Hz)

applyVolumeModulation :: Wave -> Wave -> Wave
applyVolumeModulation modulation wave = zipWith (*) wave modulation

sinVolumeModulation :: Float -> Wave -> Wave
sinVolumeModulation rate wave = applyVolumeModulation wave $ map sin $ take (length wave) [0.0 , qrate .. ]
    where
        qrate = 1/(rate * 1000)

squareVolumeModulation :: Hz -> Wave -> Wave
squareVolumeModulation freq wave = applyVolumeModulation wave $ map (signum . sin) $ take (length wave) [0, rate ..]
    where
        rate =   1/(2 * pi * freq)

squareVolumeModulation1 :: Hz -> Wave -> Wave
squareVolumeModulation1 freq wave = applyVolumeModulation wave $ map (sin . (* 9)) (take (length wave) [0, 0.001 .. ])