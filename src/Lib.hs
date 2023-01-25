module Lib
    ( someFunc
    ) where
import Pitch

type Wave = [Float]
type Volume = Float
type Sec = Float
type Samples = Float
type Hz = Float
type Phrase = [Note]
type Beat = Float
type Note = [Float]


someFunc :: IO ()
someFunc = putStrLn "someFunc"
