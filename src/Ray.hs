module Ray where

import Data.Vec3 hiding (origin)
import Vec3

data Ray = Ray { origin :: CVec3
               , direction :: CVec3
               } deriving (Show)

pointAt :: Ray -> Double -> CVec3
pointAt ray distance = origin ray <+> (direction ray .^ distance)
