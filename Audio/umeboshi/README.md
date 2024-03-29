# Umeboshi: A Haskell Drum Machine

Umeboshi uses a Roland 808 soundbank to create `.wav` files of rhythms.
The drum machine is designed to facilitate poly-rhythmic percussion in
non-standard time signatures. It relies heavily on Unboxed Vector types and the
`Data.WAVE` library. By design many drum machines constrain writing drum
patterns to 3/4 or 4/4 time, limiting the ability to write more complex rhythms
such as a pentuplet over three quarter notes. Umeboshi is an attempt to
fill the gap left by such design choices.

The two primary functions are:
```
buildMeasure :: BPM -> Signature -> [Performance] -> VectSamples
makeWavFile :: VectSamples -> IO ()
```
where
```
type Performance = (Rhythm, WAVE)
type VectSamples = U.Vector Int32
data Signature = Time Int Int
type Rhythm = String
type BPM = Float
```

`WAVE` types are derived from the Roland 808 soundbank:
`[hiTom, kick, maracas, opHiHat] <- roland808`.<br>`Rhythm` is a string composed
of `dots` and `xs`, where the `dots` act as rests and the `xs` act as attacks.

For example, given ensemble `Performances` we can build a measure of
5/4 at 120 BPM:
```
ensemble1 = [(".xx", hiTom),("xxxxx", maracas),("x", opHiHat),(".x", snare)]
m1 = buildMeasure 120 (Time 5 4) ensemble1
```
`ensemble1` has quarter note `maracas` and a `hiTom` triplet which stretches
over the measure.<br>The first beat of the triplet is a rest. 

Creating an additional 7/4 measure at 122 BPM:
```
ensemble2 = [(".x.", maracas),("x.x", rimshot),("x.", opHiHat),(".x", handClap)]
m2 = buildMeasure 122 (Time 7 4) ensemble2
```
The two measures can be combined and compiled as `temp.wav`, which is saved
to the source directory.
```
rhythm = U.concat [m1, m2]
makeWavFile rhythm
```
