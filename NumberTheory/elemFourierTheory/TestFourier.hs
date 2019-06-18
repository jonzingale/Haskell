module TestFourier where
import Data.Complex
import Fourier

-- TODO: QuickCHECK

em1 = Zn 2 6
chi0 = Chi (\a -> eval $ a * Zn 0 (ord a)) -- evaluates to 1 for all g
chi1 = Chi (\a -> eval $ a * Zn 1 (ord a))
chi2 = Chi (\a -> eval $ a * Zn 2 (ord a))

-- test functions
test_chars = zipWith (<||>) (chars em1) (elems em1) -- roots unity
test_principle = [chi0 <||> e | e <- elems em1] -- all 1s

test_sum_of_nontrivial_char = sum [ chi1 <||> g | g <- elems em1] -- should be ZERO
test_conj_inv = [(conjugate $ x <||> em1) * (x <||> em1) | x <- chars em1] -- all 1s

table elem = do -- the character table
  let rows = [x <||> e | e<- elems elem, x <- chars elem]
  putStr.unlines $ f rows
  where
    f [] = []
    f rs = ((++ "\n").show.(take 6) $ rs) : f (drop 6 rs)

verify_orthogonality_of_chars = do -- diagonals are ord em1, 0 otherwise
  let rows = [(x1 <|> x2) em1 | x1<- chars em1, x2 <- chars em1]
  putStr.unlines $ f rows
  where
    f [] = []
    f rs = ((++ "\n").show.(take 6) $ rs) : f (drop 6 rs)
