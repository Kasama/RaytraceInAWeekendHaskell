module Main where

import Scene
import Shape
import Camera
import Color
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

cameraPos = fromXYZ (13, 2, 3)
cameraLookAt = fromXYZ (0, 0, 0)
cameraVup = fromXYZ (0, 1, 0)

cam :: Camera
cam = getCamera cameraPos cameraLookAt cameraVup
                (16, 9) -- Aspect Ratio 2x1
                160    -- Scale
                20     -- FOV Degrees
                0.1    -- aperture
                10     -- distance to focus
                Perspective

scene :: Scene
scene = Scene { antialiasing = 50
              , objects      = shapes
              , camera       = cam
              , rng          = mkStdGen 0
              }

randomSphere :: (Int, Int) -> StdGen -> Shape
randomSphere (a, b) rng = Sphere { center = center
                                 , radius = 0.2
                                 , material = material
                                 }
  where
    (centerX, rngY) = random rng
    (centerZ, rngMaterialSelector) = random rngY
    center :: CVec3
    center = fromXYZ (fromIntegral a + 0.9 * centerX, 0.2, fromIntegral b + 0.9 * centerZ)
    randomDouble :: StdGen -> (Double, StdGen)
    randomDouble = random
    (materialSelector, rngMaterial) = randomDouble rngMaterialSelector
    getLambertian rng = Lambertian color
      where
        color = toXYZ (color1 `mult` color2)
        (color1, rngColor) = randomColor rng
        (color2, _) = randomColor rngColor
    getMetallic rng = Metal fuzz color
      where
        (fuzz, rngColor) = random rng
        (color, _) = randomColor rngColor
    material
      | materialSelector < 0.8 = getLambertian rngMaterial
      | materialSelector < 0.95 = getMetallic rngMaterial
      | otherwise = Dieletric 1.5



randomScene :: Int -> Scene
randomScene rngSeed = Scene { antialiasing = 1
                            , objects      = defaultSpheres ++ randomSpheres
                            , camera       = cam
                            , rng          = mkStdGen 0
                            }
  where
    rngSpheres = map mkStdGen $ randoms $ mkStdGen rngSeed
    randomSpheres = zipWith randomSphere [(a, b) | a <- [-11..11], b <- [-11..11]] rngSpheres
    defaultSpheres = [ Sphere { center = fromXYZ (4, -1000, 0), radius = 1000, material = Lambertian (0.5, 0.5, 0.5) }
                     , Sphere { center = fromXYZ (0, 1, 0),     radius = 1,    material = Dieletric 1.5 }
                     , Sphere { center = fromXYZ (-4, 1, 0),    radius = 1,    material = Lambertian (0.4, 0.2, 0.1) }
                     , Sphere { center = fromXYZ (4, 1, 0),     radius = 1,    material = Metal    0 (0.7, 0.6, 0.5) }
                     ]

main :: IO ()
main = do
          let rngSeed = 2
          let s = randomScene rngSeed
          -- let s = scene
          let nx = nPixelsHorizontal (camera s)
          let ny = nPixelsVertical (camera s)
          let scenes = map (setRng s) (randoms $ mkStdGen rngSeed)
          let pixelColors = zipWith getSceneColor scenes (screenPixels nx ny)
          let parallelPixelColors = pixelColors `using` parListChunk 16 rdeepseq
          printPPMHeader s;
          mapM_ printPixelColor parallelPixelColors
