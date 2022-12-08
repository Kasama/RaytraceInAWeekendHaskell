module Vec3 where

import           System.Random
import           Data.Vec3

-- | Linear interpolation between two vectors.
lerp :: Vec3 a => a -> a -> Double -> a
-- Calculate the weighted average of the two vectors using
-- the given interpolation parameter.
lerp a b t = a .^ (1 - t) <+> b .^ t

-- | Apply a function to each element of a three-dimensional vector.
vmap :: Vec3 a => (Double -> Double) -> a -> a
-- Convert the vector to a tuple of coordinates, apply the function
-- to each coordinate, and convert the resulting tuple back to a vector.
vmap f v = fromXYZ (f x, f y, f z) where (x, y, z) = toXYZ v

-- | Generate a random point on the unit sphere.
randomVec :: StdGen -> (CVec3, StdGen)
-- Generate three random numbers in the range (-1, 1), and
-- construct a vector with these coordinates. Return the
-- vector and the updated random number generator.
randomVec gen = (vec, nextGen)
 where
  vec          = fromXYZ (x, y, z)
  (x, genY   ) = randomR (-1, 1) gen
  (y, genZ   ) = randomR (-1, 1) genY
  (z, nextGen) = randomR (-1, 1) genZ

-- | Generate a random point on the surface of a sphere with radius 1.
randomPointInSphere :: StdGen -> (CVec3, StdGen)
-- If the generated point is on the surface of the sphere, return it
-- along with the updated random number generator. Otherwise, generate
-- a new point and try again.
randomPointInSphere gen | norm vec < 1.0 = (vec, nextGen)
                        | otherwise      = randomPointInSphere nextGen
  where
        -- Generate a random point on the unit sphere.
        (vec, nextGen) = randomVec gen


-- | Generate a random point on the surface of a disk with radius 1.
randomPointInDisk :: StdGen -> (CVec3, StdGen)
-- If the generated point is inside the disk, return it along with
-- the updated random number generator. Otherwise, generate a new point
-- and try again.
randomPointInDisk gen | norm vec < 1.0 = (vec, nextGen)
                      | otherwise      = randomPointInDisk nextGen
 where
  -- Generate a random point in the square with side length 2
  -- centered at the origin.
  vec          = fromXYZ (x, y, 0.0)
  (x, genY   ) = randomR (-1, 1) gen
  (y, nextGen) = randomR (-1, 1) genY


-- | Calculate the direction of a reflected ray given the direction
--   of the incoming ray and the normal vector at the point of intersection.
reflect :: CVec3 -> CVec3 -> CVec3
reflect view normal = view <-> (n .^ (2 * (n .* view)))
  where
        -- Normalize the normal vector at the point of intersection.
        n = normalize normal

-- | Calculate the direction of a refracted ray given the direction
--   of the incoming ray, the normal vector at the point of intersection,
--   and the refractive index of the material that the ray is entering.
refract :: CVec3 -> CVec3 -> Double -> (CVec3, Bool)
refract view normal refractionIndex = (refracted, didRefract) -- Snell's law
 where
    -- Calculate the dot product of the normalized incoming ray direction
    -- and the normal vector at the point of intersection. This represents
    -- the cosine of the angle of incidence.
  dt           = normalize view .* normal

  -- Calculate the discriminant of the quadratic equation that is used
  -- to solve for the refracted ray direction using Snell's law.
  discriminant = 1.0 - (refractionIndex * refractionIndex * (1.0 - (dt * dt)))
  -- Determine whether the ray was refracted or not based on the
  -- calculated discriminant.
  didRefract   = discriminant > 0

  -- If the ray was refracted, calculate the refracted ray direction
  -- using Snell's law. Otherwise, return a zero vector.
  refracted    = if didRefract
    then
      ((normalize view <-> (normal .^ dt)) .^ refractionIndex)
        <-> (normal .^ sqrt discriminant)
    else fromXYZ (0, 0, 0)


vMult :: CVec3 -> CVec3 -> CVec3
vMult = Data.Vec3.zipWith (*)
