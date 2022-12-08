module Ray where

import           Data.Vec3               hiding ( origin )
import           Vec3

-- | Data structure representing a ray in 3D space.
data Ray = Ray {
  -- | The origin of the ray.
  origin :: CVec3,
  -- | The direction of the ray.
  direction :: CVec3
} deriving (Show)

-- | Calculate the point at a given distance along the given ray.
pointAt :: Ray -> Double -> CVec3
pointAt ray distance = origin ray <+> (direction ray .^ distance)
