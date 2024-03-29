module Pitch where

type Hz = Float

{-
C0	16.352 	C4	261.626     C8	4186.009
C#0	17.324 	C#4	277.183 	C#8	4434.922
D0	18.354 	D4	293.665 	D8	4698.637
D#0	19.445 	D#4	311.127 	D#8	4978.032
E0	20.602 	E4	329.628 	E8	5274.042
F0	21.827 	F4	349.228 	F8	5587.652
F#0	23.125 	F#4	369.994 	F#8	5919.912
G0	24.500 	G4	391.995 	G8	6271.928
G#0	25.957 	G#4	415.305 	G#8	6644.876
A0	27.500  A4	440.000 	A8 7040.000
A#0	29.135 	A#4	466.164 	A#8	7458.620
B0	30.868 	B4	493.883 	B8	7902.133
C1	32.703 	C5	523.251 	C9	8372.019
C#1	34.648 	C#5	554.365 	C#9	8869.845
D1	36.708 	D5	587.330 	D9	9397.273
D#1	38.891 	D#5	622.254 	D#9	9956.064
E1	41.203 	E5	659.255 	E9	10548.083
F1	43.654 	F5	698.457 	F9	11175.305
F#1	46.249 	F#5	739.989 	F#9	11839.823
G1	48.999 	G5	783.991 	G9	12543.855
G#1	51.913 	G#5	830.609 	G#9	13289.752	-
A1	55.000 	A5	880.000
A#1	58.270 	A#5	932.328
B1	61.735 	B5	987.767
C2	65.406 	C6	1046.502
C#2	69.296 	C#6	1108.731
D2	73.416 	D6	1174.659
D#2	77.782 	D#6	1244.508
E2	82.407 	E6	1318.510
F2	87.307 	F6	1396.913
F#2	92.499 	F#6	1479.978
G2	97.999 	G6	1567.982
G#2	103.826 G#6	1661.219
A2	110.000 A6	1760.000
A#2	116.541 A#6	1864.655
B2	123.471 B6	1975.533
C3	130.813 C7	2093.005
C#3	138.591 C#7	2217.461
D3	146.832 D7	2349.318
D#3	155.564 D#7	2489.016
E3	164.814 E7	2637.021
F3	174.614 F7	2793.826
F#3	184.997 F#7	2959.956
G3	195.998 G7	3135.964
G#3	207.652 G#7	3322.438
A3	220.000 A7	3520.000
A#3	233.082 A#7	3729.310
B3	246.942 B7	3951.066
-}

pitches :: [Hz]
pitches =[16.352,
            17.324,
            18.354,
            19.445,
            20.602,
            21.827,
            23.125,
            24.500,
            25.957,
            27.500,
            29.135,
            30.868,
            32.703,
            34.648,
            36.708,
            38.891,
            41.203,
            43.654,
            46.249,
            48.999,
            51.913,
            55.000,
            58.270,
            61.735,
            65.406,
            69.296,
            73.416,
            77.782,
            82.407,
            87.307,
            92.499,
            97.999,
            103.826,
            110.000,
            116.541,
            123.471,
            130.813,
            138.591,
            146.832,
            155.564,
            164.814,
            174.614,
            184.997,
            195.998,
            207.652,
            220.000,
            233.082,
            246.942,
            261.626,
            277.183,
            293.665,
            311.127,
            329.628,
            349.228,
            369.994,
            391.995,
            415.305,
            440,
            466.164,
            493.883,
            523.251,
            554.365,
            587.330,
            622.254,
            659.255,
            698.457,
            739.989,
            783.991,
            830.609,
            880.000,
            932.328,
            987.767,
            1046.502,
            1108.731,
            1174.659,
            1244.508,
            1318.510,
            1396.913,
            1479.978,
            1567.982,
            1661.219,
            1760.000,
            1864.655,
            1975.533,
            2093.005,
            2217.461,
            2349.318,
            2489.016,
            2637.021,
            2793.826,
            2959.956,
            3135.964,
            3322.438,
            3520.000,
            3729.310,
            3951.066,
            4434.922,
            4698.637,
            4978.032,
            5274.042,
            5587.652,
            5919.912,
            6271.928,
            6644.876,
            7040.000,
            7458.620,
            7902.133,
            8372.019,
            8869.845,
            9397.273,
            9956.064,
            10548.083,
            11175.305,
            11839.823,
            12543.855,
            13289.752]

c0 :: Hz
c0= 16.352
cs0 :: Hz
cs0 = 17.324
d0 :: Hz
d0 =18.354
ds0 :: Hz
ds0 =19.445
e0 :: Hz
e0 =20.602
f0 :: Hz
f0 =21.827
fs0 :: Hz
fs0 =23.125
g0 :: Hz
g0 =24.500
gs0 :: Hz
gs0 =25.957
a0 :: Hz
a0 =27.500
as0 :: Hz
as0 =29.135
b0 :: Hz
b0 =30.868
c1 :: Hz
c1 =32.703
cs1 :: Hz
cs1 =34.648
d1 :: Hz
d1 =36.708
ds1 :: Hz
ds1 =38.891
e1 :: Hz
e1 =41.203
f1 :: Hz
f1 =43.654
fs1 :: Hz
fs1 =46.249
g1 :: Hz
g1 =48.999
gs1 :: Hz
gs1 =51.913
a1 :: Hz
a1 =55.000
as1 :: Hz
as1 =58.270
b1 :: Hz
b1 =61.735
c2 :: Hz
c2 =65.406
cs2 :: Hz
cs2 =69.296
d2 :: Hz
d2 =73.416
ds2 :: Hz
ds2 =77.782
e2 :: Hz
e2 =82.407
f2 :: Hz
f2 =87.307
fs2 :: Hz
fs2 =92.499
g2 :: Hz
g2 =97.999
gs2 :: Hz
gs2 =103.826
a2 :: Hz
a2 =110.000
as2 :: Hz
as2 =116.541
b2 :: Hz
b2 =123.471
c3 :: Hz
c3 =130.813
cs3 :: Hz
cs3 =138.591
d3 :: Hz
d3 =146.832
ds3 :: Hz
ds3 =155.564
e3 :: Hz
e3 =164.814
f3 :: Hz
f3 =174.614
fs3 :: Hz
fs3 =184.997
g3 :: Hz
g3 =195.998
gs3 :: Hz
gs3 =207.652
a3 :: Hz
a3 =220.000
as3 :: Hz
as3 =233.082
b3 :: Hz
b3 =246.942
c4 :: Hz
c4 =261.626
cs4 :: Hz
cs4 =277.183
d4 :: Hz
d4 =293.665
ds4 :: Hz
ds4 =311.127
e4 :: Hz
e4 =329.628
f4 :: Hz
f4 =349.228
fs4 :: Hz
fs4 =369.994
g4 :: Hz
g4 =391.995
gs4 :: Hz
gs4 =415.305
a4 :: Hz
a4 =440.000
as4 :: Hz
as4 =466.164
b4 :: Hz
b4 =493.883
c5 :: Hz
c5 =523.251
cs5 :: Hz
cs5 =554.365
d5 :: Hz
d5 =587.330
ds5 :: Hz
ds5 =622.254
e5 :: Hz
e5 =659.255
f5 :: Hz
f5 =698.457
fs5 :: Hz
fs5 =739.989
g5 :: Hz
g5 =783.991
gs5 :: Hz
gs5 =830.609
a5 :: Hz
a5 =880.000
as5 :: Hz
as5 =932.328
b5 :: Hz
b5 =987.767
c6 :: Hz
c6 =1046.502
cs6 :: Hz
cs6 =1108.731
d6 :: Hz
d6 =1174.659
ds6 :: Hz
ds6 =1244.508
e6 :: Hz
e6 =1318.510
f6 :: Hz
f6 =1396.913
fs6 :: Hz
fs6 =1479.978
g6 :: Hz
g6 =1567.982
gs6 :: Hz
gs6 =1661.219
a6 :: Hz
a6 =1760.000
as6 :: Hz
as6 =1864.655
b6 :: Hz
b6 =1975.533
c7 :: Hz
c7 =2093.005
cs7 :: Hz
cs7 =2217.461
d7 :: Hz
d7 =2349.318
ds7 :: Hz
ds7 =2489.016
e7 :: Hz
e7 =2637.021
f7 :: Hz
f7 =2793.826
fs7 :: Hz
fs7 =2959.956
g7 :: Hz
g7 =3135.964
gs7 :: Hz
gs7 =3322.438
a7 :: Hz
a7 =3520.000
as7 :: Hz
as7 =3729.310
b7 :: Hz
b7 =3951.066
c8 :: Hz
c8 =4186.009
cs8 :: Hz
cs8 =4434.922
d8 :: Hz
d8 =4698.637
ds8 :: Hz
ds8 =4978.032
e8 :: Hz
e8 =5274.042
f8 :: Hz
f8 =5587.652
fs8 :: Hz
fs8 =5919.912
g8 :: Hz
g8 =6271.928
gs8 :: Hz
gs8 =6644.876
a8 :: Hz
a8  =7040.000
as8 :: Hz
as8 =7458.620
b8 :: Hz
b8 =7902.133
c9 :: Hz
c9 =8372.019
cs9 :: Hz
cs9 =8869.845
d9 :: Hz
d9 =9397.273
ds9 :: Hz
ds9 =9956.064
e9 :: Hz
e9 =10548.083
f9 :: Hz
f9 =11175.305
fs9 :: Hz
fs9 =11839.823
g9 :: Hz
g9 =12543.855
gs9 :: Hz
gs9 =13289.752
