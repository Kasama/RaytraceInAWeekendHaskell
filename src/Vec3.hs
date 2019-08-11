module Vec3 where

import Data.Vec3

lerp :: Vec3 a => a -> a -> Double -> a
lerp a b t = a .^ (1 - t) <+> b .^ t

vmap :: Vec3 a => (Double -> Double) -> a -> a
vmap f v = fromXYZ (f x, f y, f z)
  where (x, y, z) = toXYZ v
