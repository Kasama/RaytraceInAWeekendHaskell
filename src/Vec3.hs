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
  | norm vec < 1.0 = (vec, nextGen)
  | otherwise = randomPointInSphere nextGen
  where
    vec = fromXYZ (x, y, z)
    (x, genY) = randomR (-1, 1) gen
    (y, genZ) = randomR (-1, 1) genY
    (z, nextGen) = randomR (-1, 1) genZ

reflect :: CVec3 -> CVec3 -> CVec3
reflect view normal = view <-> (n .^ (2 * (n .* view)))
  where
    n = normalize normal

vMult :: CVec3 -> CVec3 -> CVec3
vMult = Data.Vec3.zipWith (*)
