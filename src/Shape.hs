module Shape where

import Ray
import Data.Vec3 hiding (origin)

type Shape = Sphere

data Sphere = Sphere { center :: CVec3
                     , radius :: Double
                     } deriving (Show)

instance Hittable Sphere where
  hit sphere ray = if delta < 0
    then -1
    else ( -b - sqrt delta ) / (2 * a)
    where
      delta = b * b - 4 * a * c
      a = direction ray .* direction ray
      b = (oc .* direction ray) * 2
      c = oc .* oc - radius sphere * radius sphere
      oc = origin ray <-> center sphere

class Hittable a where
  hit :: a -> Ray -> Double
