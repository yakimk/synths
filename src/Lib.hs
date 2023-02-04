module Lib
    (
    Wave,
    Volume,
    Sec,
    Samples,
    Phrase,
    Hz,
    Beat,
    Note,
    WaveFunction,
    WaveTransform,
    ) where
        
type Wave = [Float]
type Volume = Float
type Sec = Float
type Samples = Float
type Hz = Float
type Phrase = [Note]
type Beat = Float
type Note = [Float]
type WaveFunction = Wave -> Wave
type WaveTransform = Float -> Float