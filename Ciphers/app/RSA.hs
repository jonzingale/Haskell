module RSA where
import qualified Data.Vector as U

type Key = Integer
type Modulus = Integer
type Message = U.Vector Integer

ekey, dkey :: Key
ekey = 1823 -- some values work
dkey = inverse modulus ekey

modulus :: Integer
modulus = 1987 -- 104161

encrypt :: Message -> Message
encrypt msg = U.map (\t -> mod (t^ekey) modulus) msg

decrypt :: Message -> Message
decrypt msg = U.map (\t -> mod (t^dkey) modulus) msg

inverse :: Modulus -> Key -> Key
inverse m a = head [b | b <- [1..m], mod (b*a) (m-1) == 1]
