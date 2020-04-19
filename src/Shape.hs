module Shape where

import Ray
import Vec3
import Color
import System.Random
import Data.Vec3 hiding (origin)

data Shape
  = Sphere { center :: CVec3
           , radius :: Double
           , material :: Material
           }
  | Shapeless
  deriving (Show)

data Material
  = Lambertian Color
  | Metal Double Color
  | Dieletric Double
  deriving (Show)

schlick :: Double -> Double -> Double
schlick cosine refractionIndex = r + (1 - r) *  ((1 - cosine) ** 5)
  where
    r = ((1 - refractionIndex) / (1 + refractionIndex)) ** 2

scatter :: Ray -> HitRecord -> StdGen -> (Ray, Color, Bool, StdGen)
scatter ray hitRecord rng = case material $ shape hitRecord of
  Lambertian color -> (nextRay, color, True, nextRng)
    where
      nextRayTarget = interception hitRecord <+> normal hitRecord <+> randomPoint
      (randomPoint, nextRng) = randomPointInSphere rng
      nextRay = Ray { origin = interception hitRecord
                    , direction = nextRayTarget <-> interception hitRecord
                    }
  Metal fuzzyness color -> (nextRay, color, didReflect, nextRng)
    where
      reflectedRay = reflect (direction ray) (normal hitRecord)
      (randomPoint, nextRng) = randomPointInSphere rng
      nextRay = Ray { origin = interception hitRecord
                    , direction = reflectedRay <+> (randomPoint .^ fuzzyness)
                    }
      didReflect = direction nextRay .* normal hitRecord > 0
  Dieletric materialRefractionIndex -> (nextRay, white, True, nextRng)
    where
      (rndSample, nextRng) = randomR (0, 1) rng

      isRayFromOutside = direction ray .* normal hitRecord > 0
      outwardNormal = if isRayFromOutside then invert $ normal hitRecord else normal hitRecord
      refractionIndex = if isRayFromOutside then materialRefractionIndex else 1.0 / materialRefractionIndex
      cosine = (direction ray .* normal hitRecord) / norm (direction ray) * if isRayFromOutside then materialRefractionIndex else (-1)

      (refractedRay, didRefract) = refract (direction ray) outwardNormal refractionIndex

      reflectionProbability = if didRefract then schlick cosine materialRefractionIndex else 1.0
      performReflection = rndSample < reflectionProbability

      reflectedRay = reflect (direction ray) (normal hitRecord)

      nextRay = Ray { origin = interception hitRecord
                    , direction = if performReflection then reflectedRay else refractedRay
                    }

data HitRecord = HitRecord { t :: Double
                           , normal :: CVec3
                           , interception :: CVec3
                           , didHit :: Bool
                           , shape :: Shape
                           } deriving (Show)

instance Eq HitRecord where
  (HitRecord t1 _ _ _ _) == (HitRecord t2 _ _ _ _) = t1 == t2

instance Ord HitRecord where
  (HitRecord t1 _ _ _ _) `compare` (HitRecord t2 _ _ _ _) = t1 `compare` t2

originVec :: CVec3
originVec = fromXYZ (0, 0, 0)

hit :: Shape -> Ray -> Double -> Double -> HitRecord
hit sphere@Sphere{} ray tMin tMax
    | delta > 0 && negativeT < tMax && negativeT > tMin = -- Hit something
      HitRecord negativeT (normal $ hitPointFor negativeT) (hitPointFor negativeT) True sphere
    | delta > 0 && positiveT < tMax && positiveT > tMin =
      HitRecord positiveT (normal $ hitPointFor positiveT) (hitPointFor positiveT) True sphere
    | otherwise = -- Hit nothing
      HitRecord (-1) originVec originVec False Shapeless
    where
      delta = b * b - 4 * a * c
      a = direction ray .* direction ray
      b = (oc .* direction ray) * 2
      c = oc .* oc - radius sphere * radius sphere
      t op = ( (-b) `op` sqrt delta) / (2 * a)
      positiveT = t (+)
      negativeT = t (-)
      hitPointFor = pointAt ray
      normal hitPoint = (hitPoint <-> center sphere) .^ (1 / radius sphere)
      oc = origin ray <-> center sphere
