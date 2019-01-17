module Compiler.Config (
    intMax,
    intMin

) where
-- | The max bound for integer out of range warnings
-- Note that we don't use maxBound Int here simply because VMs and
-- assemblers will have different bound. The concept of a maximum or
-- minimum bound of an integer is not even specified in the CiviC
-- specification, however the virtual machine does have a bound.
intMax :: Integer
intMax = 2^31 - 1

-- | The min bound for integer out of range warnings.
intMin :: Integer
intMin = -(2^31)
