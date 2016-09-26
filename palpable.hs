data Plus = P | E deriving (Show)
data Protrudent a = Pro Plus a Plus deriving (Show)
data Hardness a = Hard Plus a Plus deriving (Show)
data Size a = Size Plus a Plus deriving (Show)

data TriggerPoint a = TrP (Protrudent a) (Hardness a ) (Size a) deriving (Show)

trigger_point = TrP (Pro P 2 E) (Hard P 1 P) (Size P 1 E)

data Tissue a = Muscle | Bone | TriggerPoint a | Tendon deriving (Show)

data Adhesion a = Adh (Tissue a) (Tissue a) deriving (Show)

tibia_tibialis = Adh Bone Muscle

first_pair :: Adhesion a -> Tissue a
first_pair (Adh a b) = a

second_pair :: Adhesion a -> Tissue a
second_pair (Adh a b) = b
