![alt text](https://i1.sndcdn.com/artworks-WQvrMiCTRSy1bMXi-7EvKhA-t500x500.jpg "crochet peptides")


__Pandemic: A peptide symphony in A Major__

Pandemic Peptide Symphony In A Major is a composition produced from the cDNA
sequence for covid-19 and was created as a submission for a November computer
music challenge. Each peptide in the sequence determines a melody by sending
each amino acid to a note in a 20 tone system. Acids are ordered by their
abundance across all peptides and similarly, the intervals are ordered by their
abundance wrt harmonics on a string. The most common acid then maps to the tonic
tone, the second most to the 5th (closest approximation in a 20 tone system),
etc...

Since the codons carry more information than simply which amino acid they code
for (a triple of bases), this additional information is used to determine
additional qualities for a given note. The 2nd base determines the "epoch"
or the time allotted for a given note to express, while the 3rd base determines
how long the note expresses. For instance, the codons CGG and AGA both code for
the acid Arginine. The epoch value "G" determines a half note worth of space for
the note to express. In the CGG case, that note expresses for the full time. In
the AGA case, "A" corresponds to an eighth note and so the note expresses for
the duration of an eighth note and then is followed by a dotted quarter note
rest. If the 3rd pair corresponds to a longer duration than the epoch, the time
is truncated down to match the epoch's duration. Presently, the base type to
duration mapping is arbitrary.

Once determined, the sounds are compiled along with a few aesthetic choices
such as octave, long tones, volume, panning, et cetera. The "sounds" are stored
as 32 bit integer arrays, mixed and saved as WAVE files.

I would like to thank Ken Prokuski and Ed Angel for their camaraderie and
support in making this project possible. Thank you also to Joe Moon and Toshia
Myers for the helpful discussions about genomics.

The final piece can be listened to here:
[Pandemic: A peptide symphony in A Major](https://soundcloud.com/jeejaws-for-jawaas/pandemic-peptide-symphony-in-a-maj)