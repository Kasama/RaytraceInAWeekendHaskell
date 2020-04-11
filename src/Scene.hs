module Scene where

import Data.Vec3
import Shape
import System.Random

type UV = (Double, Double)

data Scene = Scene { aspectRatioH :: Integer
                   , aspectRatioV :: Integer
                   , scale :: Integer
                   , antialiasing :: Integer
                   , objects :: [Sphere]
                   , rng :: StdGen
                   }

setRng :: Scene -> Int -> Scene
setRng s rngSeed = Scene
                     (aspectRatioH s)
                     (aspectRatioV s)
                     (scale s)
                     (antialiasing s)
                     (objects s)
                     (mkStdGen rngSeed)

nPixelsHorizontal scene = scale scene * aspectRatioH scene
nPixelsVertical   scene = scale scene * aspectRatioV scene
toUV              scene (x, y) = (u, v) :: UV
  where u = x / fromInteger (nPixelsHorizontal scene)
        v = y / fromInteger (nPixelsVertical scene)
horizontal        scene = fromXYZ (fromInteger $ 2 * aspectRatioH scene, 0, 0)
vertical          scene = fromXYZ (0, fromInteger $ 2 * aspectRatioV scene, 0)
lowerLeftCorner   scene = fromXYZ ( fromInteger $ -(aspectRatioH scene)
                                  , fromInteger $ -(aspectRatioV scene)
                                  , -1
                                  )
