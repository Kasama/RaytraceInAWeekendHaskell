module Ray where

import Data.Vec3 hiding (origin)
import Vec3

data VisionRay = VisionRay { rayOrigin :: CVec3
                           , rayDirection :: CVec3
                           } deriving (Show)

class Ray a where
  origin :: a -> CVec3
  direction :: a ->  CVec3
  pointAt :: a -> Double -> CVec3

instance Ray VisionRay where
  origin = rayOrigin
  direction = rayDirection
  pointAt ray distance = origin ray <+> (direction ray .^ distance)
