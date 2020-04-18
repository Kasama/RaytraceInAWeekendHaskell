module Camera where

import Data.Vec3

type Scale = Integer
type AspectRatio = (Integer, Integer)
horizontalAspect :: AspectRatio -> Integer
horizontalAspect (h, _) = h
verticalAspect :: AspectRatio -> Integer
verticalAspect (_, v) = v

data Camera = Camera { aspectRatio :: AspectRatio
                     , scale :: Scale
                     , position :: CVec3
                     , horizontal :: CVec3
                     , vertical :: CVec3
                     , lowerLeftCorner :: CVec3
                     }

getCamera :: CVec3 -> AspectRatio -> Scale -> Camera
getCamera pos aspect@(horizontalAspect, verticalAspect) scale = Camera
  aspect
  scale
  pos
  (fromXYZ (fromInteger $ 2 * horizontalAspect, 0, 0))
  (fromXYZ (0, fromInteger $ 2 * verticalAspect, 0))
  (fromXYZ ( fromInteger $ -horizontalAspect, fromInteger $ -verticalAspect, -1))

type UV = (Double, Double)

nPixelsHorizontal camera = scale camera * horizontalAspect (aspectRatio camera)
nPixelsVertical   camera = scale camera * verticalAspect (aspectRatio camera)
toUV              camera (x, y) = (u, v) :: UV
  where u = x / fromInteger (nPixelsHorizontal camera)
        v = y / fromInteger (nPixelsVertical camera)

getRay :: Camera -> UV -> CVec3
getRay cam (u, v)= lowerLeftCorner cam <+> (horizontal cam .^ u) <+> (vertical cam .^ v)

reflect :: CVec3 -> CVec3 -> CVec3
reflect view normal = view <-> (normal .^ (2 * (normal .* view)))
