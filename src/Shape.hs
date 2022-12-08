module Shape where

import Ray
import Vec3
import Color
import System.Random
import Data.Maybe
import Data.Vec3 hiding (origin)

data Shape
  = Sphere { center :: CVec3
           , radius :: Double
           , material :: Material
           }
  | Shapeless
  deriving (Show)

data Material
  -- Diffuse
  = Lambertian Color
  -- Reflective
  | Metal Double Color
  -- Transparent
  | Dieletric Double
  -- For debug. Shows object normals
  | Normal
  deriving (Show)

-- | Calculate the probability of light being reflected off a surface
--   given the angle of incidence and the refractive index of the material.
schlick :: Double -> Double -> Double
schlick cosine refractionIndex = r + (1 - r) *  ((1 - cosine) ** 5)
  where
    -- Calculate the probability of reflection for a ray of light
    -- that is perpendicular to the surface.
    r = ((1 - refractionIndex) / (1 + refractionIndex)) ** 2
    -- Interpolate between the probabilities of reflection for
    -- perpendicular and parallel incidence to calculate the
    -- probability of reflection for the given angle of incidence.

-- | Calculate the ray and color of light emitted by an object
--   after it is hit by a given ray.
scatter :: Ray -> HitRecord -> StdGen -> (Ray, Color, StdGen)
scatter ray hitRecord rng = case material $ shape hitRecord of
  -- If the material is Lambertian (diffuse), use a diffuse scattering model
  -- to calculate the emitted ray and color.
  Lambertian color -> (nextRay, color, nextRng)
    where
      -- Calculate the target point for the emitted ray by adding the
      -- intersection point, the normal vector at the intersection,
      -- and a random point within a sphere centered at the origin.
      nextRayTarget = interception hitRecord <+> normal hitRecord <+> randomPoint
      (randomPoint, nextRng) = randomPointInSphere rng
      -- Create the emitted ray by setting its origin at the intersection
      -- point and its direction towards the target point.
      nextRay = Ray { origin = interception hitRecord
                    , direction = nextRayTarget <-> interception hitRecord
                    }

  -- If the material is Metal (reflective), use a reflection model
  -- to calculate the emitted ray and color.
  Metal fuzzyness color -> (nextRay, color, nextRng)
    where
      -- Reflect the direction of the incoming ray about the normal vector
      -- at the point of intersection.
      reflectedRay = reflect (direction ray) (normal hitRecord)
      -- Add a random point within a sphere centered at the origin to the
      -- reflected ray direction, scaled by the fuzzyness of the material.
      (randomPoint, nextRng) = randomPointInSphere rng
      nextRay = Ray { origin = interception hitRecord
                    , direction = reflectedRay <+> (randomPoint .^ fuzzyness)
                    }

  -- If the material is Dieletric (transparent), use a refraction model
  -- to calculate the emitted ray and color.
  Dieletric materialRefractionIndex -> (nextRay, white, nextRng)
    where
      -- Generate a random sample in the range [0, 1].
      (rndSample, nextRng) = randomR (0, 1) rng

      -- Determine if the incoming ray is coming from outside or inside the object.
      isRayFromOutside = direction ray .* normal hitRecord > 0
      -- If the ray is coming from inside the object, invert the normal vector
      -- at the point of intersection so that it points outward.
      outwardNormal = if isRayFromOutside then invert $ normal hitRecord else normal hitRecord
      -- If the ray is coming from inside the object, the refractive index
      -- of the material must be inverted to account for the change in direction.
      refractionIndex = if isRayFromOutside then materialRefractionIndex else 1.0 / materialRefractionIndex
      -- Calculate the cosine of the angle between the incoming ray direction
      -- and the normal vector at the point of intersection.
      -- If the ray is coming from inside the object, the cosine must be negated
      -- to account for the change in direction.
      cosine = (direction ray .* normal hitRecord) / norm (direction ray) * if isRayFromOutside then materialRefractionIndex else (-1)

      -- Calculate the refracted ray using the refraction formula and the calculated refractive index and cosine.
      (refractedRay, didRefract) = refract (direction ray) outwardNormal refractionIndex

      -- Calculate the probability that the ray will be reflected based on the
      -- calculated cosine and refractive index of the material.
      reflectionProbability = if didRefract then schlick cosine materialRefractionIndex else 1.0
      -- Determine whether the ray should be reflected or refracted based on the
      -- calculated reflection probability and the random sample.
      performReflection = rndSample < reflectionProbability

      -- Reflect the incoming ray direction about the normal vector at the
      -- point of intersection.
      reflectedRay = reflect (direction ray) (normal hitRecord)

      -- Create the emitted ray by setting its origin at the point of intersection
      -- and its direction to the reflected or refracted ray, depending on
      -- the result of the reflection probability check.
      nextRay = Ray { origin = interception hitRecord
                    , direction = if performReflection then reflectedRay else refractedRay
                    }

  -- If the material is Normal (used for debug), return a ray with a
  -- direction equal to the normal vector at the point of intersection.
  -- The color of the emitted ray is set to the normalized normal vector,
  -- scaled by 0.5 and mapped to the [0, 1] range.
  Normal -> (orr, color, rng)
    where
      orr = Ray (fromXYZ (0,0,0)) (fromXYZ (0,0,0))
      color = toXYZ $ vmap (1 +) (normal hitRecord) .^ 0.5

data HitRecord = HitRecord { t :: Double
                           , normal :: CVec3
                           , interception :: CVec3
                           , shape :: Shape
                           } deriving (Show)

instance Eq HitRecord where
  (HitRecord t1 _ _ _) == (HitRecord t2 _ _ _) = t1 == t2

instance Ord HitRecord where
  (HitRecord t1 _ _ _) `compare` (HitRecord t2 _ _ _) = t1 `compare` t2

originVec :: CVec3
originVec = fromXYZ (0, 0, 0)

-- | Calculate whether a given ray intersects a given shape, and if so,
--   return information about the point of intersection.
hit :: Shape -> Ray -> Double -> Double -> Maybe HitRecord
hit sphere@Sphere{} ray tMin tMax
    -- Check if the ray intersects the sphere.
    -- If the discriminant is negative, there are no solutions,
    -- so the ray does not intersect the sphere.
    | delta > 0 && negativeT < tMax && negativeT > tMin = -- Hit something
      -- If the ray intersects the sphere at one point,
      -- return a HitRecord with information about the point of intersection.
      Just $ HitRecord negativeT (normal $ hitPointFor negativeT) (hitPointFor negativeT) sphere
    | delta > 0 && positiveT < tMax && positiveT > tMin =
      -- If the ray intersects the sphere at two points,
      -- return the HitRecord corresponding to the closer intersection.
      Just $ HitRecord positiveT (normal $ hitPointFor positiveT) (hitPointFor positiveT) sphere
    | otherwise = Nothing
    where
      -- Calculate the coefficients of the quadratic equation
      -- that represents the intersection of the ray and the sphere.
      a = direction ray .* direction ray
      b = (oc .* direction ray) * 2
      c = oc .* oc - radius sphere * radius sphere
      delta = b * b - 4 * a * c

      -- Use the quadratic formula to solve for the possible values of t at
      -- which the ray intersects the sphere.
      t op = ( (-b) `op` sqrt delta) / (2 * a)
      positiveT = t (+)
      negativeT = t (-)

      -- Calculate the point of intersection corresponding to a given value of t.
      hitPointFor = pointAt ray

      -- Calculate the normal vector at the point of intersection.
      normal hitPoint = (hitPoint <-> center sphere) .^ (1 / radius sphere)

      -- Calculate the difference between the origin of the ray and the center of the sphere.
      oc = origin ray <-> center sphere
