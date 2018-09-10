module Chemistry where

-- Mass
type Amu = Double
type Grams = Double
type GramsPerMole = Double

-- Count
type Atoms = Integer

nitrogen :: Amu
nitrogen = 14.01

hydrogen :: Amu
hydrogen = 1.008

carbon :: Amu
carbon = 12.001

oxygen :: Amu
oxygen = 16.0

amuToGrams :: Amu -> Grams
amuToGrams x = x * 1.661 * 10^^(-24)

mole :: Atoms
mole = round $ 6.022 * 10^^23

-- Molar mass (in grams) is always equal to the atomic weight of the atom!
molarMass :: Amu -> GramsPerMole -- amu -> grams/mole
molarMass amu = amuToGrams amu * (fromInteger mole)

urea = "C H4 N2 O"

molarMassUrea :: GramsPerMole
molarMassUrea =
  let mm = molarMass in
  mm carbon + 4 * mm hydrogen + 2 * mm nitrogen + mm oxygen

test =
  let amus = carbon + 4 * hydrogen + 2 * nitrogen + oxygen in
  round molarMassUrea == round amus