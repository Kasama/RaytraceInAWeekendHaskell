module Camera where

import Ray
import Vec3
import Data.Vec3
import System.Random

type Scale = Integer
type Aspect = (Integer, Integer)
horizontalAspect :: Aspect -> Integer
horizontalAspect (h, _) = h
verticalAspect :: Aspect -> Integer
verticalAspect (_, v) = v
aspectRatio :: Aspect -> Double
aspectRatio (h, w) = fromInteger h / fromInteger w

data Camera = Camera { aspect :: Aspect
                     , scale :: Scale
                     , position :: CVec3
                     , horizontal :: CVec3
                     , vertical :: CVec3
                     , lowerLeftCorner :: CVec3
                     , lensRadius :: Double
                     , uVector :: CVec3
                     , vVector :: CVec3
                     }

getCamera :: CVec3 -> CVec3 -> CVec3 -> Aspect -> Scale -> Double -> Double -> Double -> Camera
getCamera pos lookAt vUp aspect scale fov aperture focusDistance = Camera {
    aspect = aspect
  , scale = scale
  , position = pos
  , horizontal = u .^ (2 * focusDistance * halfWidth)
  , vertical = v .^ (2 * focusDistance * halfHeight)
  , lowerLeftCorner = pos <-> u .^ (halfWidth * focusDistance) <-> v .^ (halfHeight * focusDistance) <-> w .^ focusDistance
  , lensRadius = aperture / 2
  , uVector = u
  , vVector = v
  }
  where
    fovTheta = fov * pi / 180
    halfHeight = tan $ fovTheta / 2
    halfWidth = aspectRatio aspect * halfHeight
    u = normalize $ vUp >< w
    v = w >< u
    w = normalize $ pos <-> lookAt

type UV = (Double, Double)

nPixelsHorizontal camera = scale camera * horizontalAspect (aspect camera)
nPixelsVertical   camera = scale camera * verticalAspect (aspect camera)
toUV              camera (x, y) = (u, v) :: UV
  where u = x / fromInteger (nPixelsHorizontal camera)
        v = y / fromInteger (nPixelsVertical camera)

getRay :: Camera -> UV -> StdGen -> (Ray, StdGen)
getRay cam (u, v) rng = (Ray { Ray.origin = position cam <+> offset
                             , direction = lowerLeftCorner cam <+> (horizontal cam .^ u) <+> (vertical cam .^ v) <-> position cam <-> offset
                             }
                        , nextRng
                        )
  where
    (randomPoint, nextRng) = randomPointInDisk rng
    (offX, offY, _) = toXYZ $ randomPoint .^ lensRadius cam
    offset = uVector cam .^ offX <+> vVector cam .^ offY
