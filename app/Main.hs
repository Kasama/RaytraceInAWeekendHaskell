module Main where

import Scene
import Shape
import Camera
import Control.Parallel.Strategies
import Data.Vec3 hiding (zipWith)
import Data.Time.Clock.POSIX
import System.Random
import Draw ( getSceneColor
            , printPPMHeader
            , printPixelColor
            )
import Debug.Trace

rnd :: [Double]
rnd = randoms $ mkStdGen 0

screenPixels :: Integer -> Integer -> [(Integer, Integer)]
screenPixels nx ny = [ (x, y)
                     | y <- [ny-1,ny-2..0]
                     , x <- [0..nx-1]
                     ]

shapes :: [Shape]
shapes = [ Sphere { center = fromXYZ (0, 0, -1),      radius = 0.5,   material = Lambertian (0.1, 0.2, 0.5) }
         , Sphere { center = fromXYZ (0, -100.5, -1), radius = 100,   material = Lambertian (0.8, 0.8, 0.0) }
         , Sphere { center = fromXYZ (1, 0, -1),      radius = 0.5,   material = Metal  0.2 (0.8, 0.6, 0.2) }
         , Sphere { center = fromXYZ (-1, 0, -1),     radius = 0.5,   material = Dieletric 1.5 }
         , Sphere { center = fromXYZ (-1, 0, -1),     radius = -0.45, material = Dieletric 1.5 }
         ]

cameraPos = fromXYZ (3, 3, 2)
-- cameraPos = fromXYZ (0, 0, 0)
cameraLookAt = fromXYZ (0, 0, -1)
cameraVup = fromXYZ (0, 1, 0)

cam :: Camera
cam = getCamera cameraPos cameraLookAt cameraVup
                (2, 1) -- Aspect Ratio 2x1
                200    -- Scale
                20     -- FOV Degrees
                1.0    -- aperture
                (norm $ cameraPos <-> cameraLookAt) -- distance to focus

scene :: Scene
scene = Scene { antialiasing = 5
              , objects      = shapes
              , camera       = cam
              , rng          = mkStdGen 0
              }

main :: IO ()
main = do
          let rngSeed = 0
          let s = scene
          let nx = nPixelsHorizontal (camera s)
          let ny = nPixelsVertical (camera s)
          let scenes = map (setRng s) (randoms $ mkStdGen rngSeed)
          let pixelColors = zipWith getSceneColor scenes (screenPixels nx ny)
          let parallelPixelColors = pixelColors `using` parListChunk 16 rdeepseq
          printPPMHeader s;
          mapM_ printPixelColor parallelPixelColors
