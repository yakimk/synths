module VolumeModulator(sinVolumeModulation, squareVolumeModulation)
 where

import Lib (Wave, Hz)

applyVolumeModulation :: [Float] -> Wave -> Wave
applyVolumeModulation modulation wave = zipWith (*) wave modulation

sinVolumeModulation :: Float -> Wave -> Wave
sinVolumeModulation rate wave = applyVolumeModulation wave $ map sin $ take (length wave) [0.0 , qrate .. ]
    where
        qrate = 1/(rate * 1000)

squareVolumeModulation :: Hz -> Wave -> Wave
squareVolumeModulation freq wave = applyVolumeModulation wave $ map (signum . sin) $ take (length wave) [0, rate ..]
    where
        rate =   1/(2 * pi * freq)