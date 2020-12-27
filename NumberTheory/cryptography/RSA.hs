module RSA where
import Data.Numbers.Primes (isPrime, primeFactors, primes, wheelSieve)
import Math.NumberTheory.Primes.Factorisation (sieveTotient, sieveFactor,
  stepFactorisation, factorise, smallFactors, curveFactorisation)

{--
TODO:
* select functions should choose at random
* keep this file as example and build out
* optimize for larger numbers
--}

-- text
yes = 24*26^2 + 4*26 + 18

-- select primes p, q
p1 = 167
p2 = 281

-- select 𝛼 relatively prime to φ(pq).
ee = let bb a = gcd a (phi p1 p2) == 1 in
  head $ filter bb [2..phi p1 p2]

-- find multiplicative inverse, β, β = inv(𝛼) mod φ(pq).
dd = let bb a = mod (a * ee) (phi p1 p2) == 1 in
  head $ filter bb [2..phi p1 p2]

-- calculate φ for primes p, q.
phi p q = (p - 1) * (q - 1)

-- encipher text, t, by t^𝛼 mod pq.
enc t = t^ee `mod` (p1 * p2)

-- decipher enciphered text, s, by s^β mod pq.
dec s = s^dd `mod` (p1 * p2)

-- test encode and then decode.
test = yes == (dec.enc) yes
