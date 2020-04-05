# Umeboshi: A Haskell Drum Machine

Umeboshi uses a Roland 808 soundbank to create `.wav` files of rhythms.<br>
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
`[hiTom, kick, maracas, opHiHat] <- roland808`<br>`Rhythm` is a string composed
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
ensemble3 = [(".x.", maracas),("x.x", rimshot),("x.", opHiHat),(".x", handClap)]
m2 = buildMeasure 122 (Time 7 4) ensemble2
```
The two measures can be combined and compiled as `temp.wav`, which is saved
to the source directory.
```
rhythm = U.concat [m2, m2]
makeWavFile rhythm
```
