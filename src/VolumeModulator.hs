module VolumeModulator where

type Wave = [Float]

sinVolumeModulation :: Float -> Wave -> Wave
sinVolumeModulation rate wave = zipWith (*) wave $ map sin $ take (length wave) [0.0 , qrate .. ]
    where
        qrate = 1/(rate * 1000)
