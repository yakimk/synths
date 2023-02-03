module Oscillation
(squareOscillator1,
squareOscillator,
sinOscillator)
 where

import          Lib (Hz, Wave)

applyVolumeModulation :: Wave -> Wave -> Wave
applyVolumeModulation modulation wave = zipWith (*) wave modulation

sinOscillator :: Float -> Wave -> Wave
sinOscillator freq wave = applyVolumeModulation wave $
                         map sin 
                         $ take (length wave) [0.0 , rate .. ]
    where
        rate = 1/(freq * 1000)

squareOscillator :: Hz -> Wave -> Wave
squareOscillator freq wave = applyVolumeModulation wave 
                                $ map (signum . sin) 
                                $ take (length wave) [0, rate ..]
    where
        rate =   1/(2 * pi * freq)

squareOscillator1 :: Hz -> Wave -> Wave
squareOscillator1 freq wave = applyVolumeModulation wave 
                                $ map (sin . (* 9)) (take (length wave) [0, rate .. ])
    where
        rate = freq/5000