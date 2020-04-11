module Shape where

import Ray
import Data.Vec3 hiding (origin)

type Shape = Sphere

data Sphere = Sphere { center :: CVec3
                     , radius :: Double
                     } deriving (Show)

data HitRecord = HitRecord { t :: Double
                           , normal :: CVec3
                           , interception :: CVec3
                           , didHit :: Bool
                           }

instance Eq HitRecord where
  (HitRecord t1 _ _ _) == (HitRecord t2 _ _ _) = t1 == t2

instance Ord HitRecord where
  (HitRecord t1 _ _ _) `compare` (HitRecord t2 _ _ _) = t1 `compare` t2

originVec :: CVec3
originVec = fromXYZ (0, 0, 0)

instance Hittable Sphere where
  hit sphere ray tMin tMax
    | delta > 0 && negativeT < tMax && negativeT > tMin = -- Hit something
      HitRecord negativeT (normal $ hitPointFor negativeT) (hitPointFor negativeT) True
    | delta > 0 && positiveT < tMax && positiveT > tMin =
      HitRecord positiveT (normal $ hitPointFor positiveT) (hitPointFor positiveT) True
    | otherwise = -- Hit nothing
      HitRecord (-1) originVec originVec False
    where
      delta = b * b - 4 * a * c
      a = direction ray .* direction ray
      b = (oc .* direction ray) * 2
      c = oc .* oc - radius sphere * radius sphere
      t op = ( (-b) `op` sqrt delta) / (2 * a)
      positiveT = t (+)
      negativeT = t (-)
      hitPointFor = pointAt ray
      normal hitPoint = normalize $ hitPoint <-> center sphere
      oc = origin ray <-> center sphere

class Hittable a where
  hit :: a -> Ray -> Double -> Double -> HitRecord
