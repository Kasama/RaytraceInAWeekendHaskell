module Vec3 where

import Data.Vec3

lerp :: Vec3 a => a -> a -> Double -> a
lerp a b t = a .^ (1 - t) <+> b .^ t
