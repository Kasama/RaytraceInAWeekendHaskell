module Vec3 where

import System.Random
import Data.Vec3

lerp :: Vec3 a => a -> a -> Double -> a
lerp a b t = a .^ (1 - t) <+> b .^ t

vmap :: Vec3 a => (Double -> Double) -> a -> a
vmap f v = fromXYZ (f x, f y, f z)
  where (x, y, z) = toXYZ v

randomPointInSphere :: StdGen -> (CVec3, StdGen)
randomPointInSphere gen
  | x*x + y*y + z*z < 1.0 = (fromXYZ (x, y, z), nextGen)
  | otherwise = randomPointInSphere nextGen
  where
    (x, genY) = randomR (-1, 1) gen
    (y, genZ) = randomR (-1, 1) genY
    (z, nextGen) = randomR (-1, 1) genZ
