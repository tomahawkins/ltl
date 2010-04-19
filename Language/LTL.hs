-- | Using linear temporal logic (LTL) to verify embedded software and hardware.
module Language.LTL
  (
  -- * Formula Combinators
    N, B, R, F, E
  -- * Verification Directives
  , Directive (..)
  , checkVCD
  ) where

import Data.VCD

data N -- Number phantom type.
data B -- Boolean phantom type.
data R -- Regular expression phantom type.
data F -- Formula phantom type.

-- | Class of booleans or formulas.
class BF a
instance BF B
instance BF F

-- | Class of booleans or regular expressions.
class BR a
instance BR B
instance BR R

-- | Class of numbers or booleans.
class NB a
instance NB N
instance NB B

-- | LTL (and other) expressions.
data E a where
  Var      :: NB a => [String] -> E a
  Add      :: E N -> E N -> E N
  Sub      :: E N -> E N -> E N
  Mul      :: E N -> E N -> E N
  Eq       :: NB a => E a -> E a -> E B
  Lt       :: E N -> E N -> E B
  Sequence :: (BR a, BR b) => E a -> E b -> E R
  Infusion :: (BR a, BR b) => E a -> E b -> E R
  Not      :: BF a => E a -> E a
  And      :: E a -> E a -> E a
  Or       :: E a -> E a -> E a
  Empty    :: E R
  Many     :: BR a => E a -> E R
  Formula' :: BR a => E a -> E F
  Formula  :: BR a => E a -> E F
  Next'    :: E F -> E F
  Until'   :: E F -> E F -> E F
  Imply    :: E R -> E F

-- | Verification directives.
data Directive
  = Assert String (E F)  -- ^ Property must be true.
  | Assume String (E F)  -- ^ Property is assumed to be true.  Becomes an assertion in simulation.
  | Cover  String (E R)  -- ^ Sequence must be excited.

-- | Check VCD data against a set of verification directives.  Returns a list of violations with time of failure (Just: safety violation, Nothing: liveness violation).
checkVCD :: String -> Int -> [Directive] -> [(Directive, Maybe Int)]
checkVCD vcd period formulas = []
  where
  VCD _ defs samples = parseVCD vcd

