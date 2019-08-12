module Scene where

import Data.Vec3
import Shape

type UV = (Double, Double)

data Scene = Scene { aspectRatioH :: Integer
                   , aspectRatioV :: Integer
                   , scale :: Integer
                   , objects :: [Sphere]
                   }

nPixelsHorizontal scene = scale scene * aspectRatioH scene
nPixelsVertical   scene = scale scene * aspectRatioV scene
toUV              scene (x, y) = (u, v) :: UV
  where u = fromInteger x / fromInteger (nPixelsHorizontal scene)
        v = fromInteger y / fromInteger (nPixelsVertical scene)
horizontal        scene = fromXYZ (fromInteger $ 2 * aspectRatioH scene, 0, 0)
vertical          scene = fromXYZ (0, fromInteger $ 2 * aspectRatioV scene, 0)
lowerLeftCorner   scene = fromXYZ ( fromInteger $ -(aspectRatioH scene)
                                  , fromInteger $ -(aspectRatioV scene)
                                  , -1
                                  )
