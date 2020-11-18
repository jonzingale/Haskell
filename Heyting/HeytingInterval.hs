module HeytingInterval where

data Interval = Zero | Half | One

class Heyting h where
  (&&&) :: h -> h -> h
  (|||) :: h -> h -> h
  implies :: h -> h -> h
  hNot :: h -> h

instance Heyting Interval where
  (&&&) Zero Zero = Zero
  (&&&) Zero Half = Zero
  (&&&) Zero One = Zero

  (&&&) Half Zero = Zero
  (&&&) Half Half = Half
  (&&&) Half One = Half

  (&&&) One Zero = Zero
  (&&&) One Half = Half
  (&&&) One One = One

  (|||) Zero Zero = Zero
  (|||) Zero Half = Half
  (|||) Zero One = One

  (|||) Half Zero = Half
  (|||) Half Half = Half
  (|||) Half One = One

  (|||) One Zero = One
  (|||) One Half = One
  (|||) One One = One

  implies Zero Zero = One
  implies Zero Half = One
  implies Zero One = One

  implies Half Zero = Zero
  implies Half Half = One
  implies Half One = One

  implies One Zero = Zero
  implies One Half = Half
  implies One One = One

  hNot Zero = One
  hNot Half = Zero
  hNot One = Zero