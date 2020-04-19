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

rnd :: [Double]
rnd = randoms $ mkStdGen 0

screenPixels :: Integer -> Integer -> [(Integer, Integer)]
screenPixels nx ny = [ (x, y)
                     | y <- [ny-1,ny-2..0]
                     , x <- [0..nx-1]
                     ]

shapes :: [Shape]
shapes = [ Sphere { center = fromXYZ (0, 0, -1),      radius = 0.5, material = Lambertian (0.8, 0.3, 0.3) }
         , Sphere { center = fromXYZ (0, -100.5, -1), radius = 100, material = Lambertian (0.8, 0.8, 0.0) }
         , Sphere { center = fromXYZ (1, 0, -1),      radius = 0.5, material = Metal    1 (0.8, 0.6, 0.2) }
         , Sphere { center = fromXYZ (-1, 0, -1),     radius = 0.5, material = Metal  0.3 (0.8, 0.8, 0.8) }
         ]

cam :: Camera
cam = getCamera (fromXYZ (0, 0, 0))
                (2, 1) -- Aspect Ratio 2x1
                200    -- Scale

scene :: Scene
scene = Scene { antialiasing = 50
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
