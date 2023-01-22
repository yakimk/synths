volume :: Float
volume = 0.5

type Wave = [Float]

step :: Float
step = 0.01

soundPrep :: Wave -> Wave
soundPrep = map (sin . (* step)) 

wave :: [Float]
wave = soundPrep [0.0 .. 48000]