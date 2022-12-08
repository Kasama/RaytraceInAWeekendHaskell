module Main where

import           Scene
import           Shape
import           Camera
import           Color
import           Control.Parallel.Strategies
import           Data.Vec3               hiding ( zipWith )
import           Data.Time.Clock.POSIX
import           System.Random
import           Draw                           ( getSceneColor
                                                , printPPMHeader
                                                , printPixelColor
                                                )
import           Debug.Trace

rnd :: [Double]
rnd = randoms $ mkStdGen 0

screenPixels :: Integer -> Integer -> [(Integer, Integer)]
screenPixels nx ny =
  [ (x, y) | y <- [ny - 1, ny - 2 .. 0], x <- [0 .. nx - 1] ]

shapes :: [Shape]
shapes =
  [ Sphere { center   = fromXYZ (0, 0, -1)
           , radius   = 0.5
           , material = Lambertian (0.1, 0.2, 0.5)
           }
  , Sphere { center   = fromXYZ (0, -100.5, -1)
           , radius   = 100
           , material = Lambertian (0.8, 0.8, 0.0)
           }
  , Sphere { center   = fromXYZ (1, 0, -1)
           , radius   = 0.5
           , material = Metal 0.2 (0.8, 0.6, 0.2)
           }
  , Sphere { center   = fromXYZ (-1, 0, -1)
           , radius   = 0.5
           , material = Dieletric 1.5
           }
  , Sphere { center   = fromXYZ (-1, 0, -1)
           , radius   = -0.45
           , material = Dieletric 1.5
           }
  ]

cameraPos = fromXYZ (13, 2, 3)
cameraLookAt = fromXYZ (0, 0, 0)
cameraVup = fromXYZ (0, 1, 0)

cam :: Camera
cam = getCamera cameraPos
                cameraLookAt
                cameraVup
                (16, 9) -- Aspect Ratio 2x1
                50     -- Scale
                20     -- FOV Degrees
                0.1    -- aperture
                10     -- distance to focus
                Perspective

scene :: Scene
scene =
  Scene { antialiasing = 4, objects = shapes, camera = cam, rng = mkStdGen 0 }

randomSphere :: (Int, Int) -> StdGen -> Shape
randomSphere (a, b) rng = Sphere { center   = center
                                 , radius   = 0.2
                                 , material = material
                                 }
 where
  (centerX, rngY               ) = random rng
  (centerZ, rngMaterialSelector) = random rngY

  center :: CVec3
  center = fromXYZ
    (fromIntegral a + 0.9 * centerX, 0.2, fromIntegral b + 0.9 * centerZ)

  randomDouble :: StdGen -> (Double, StdGen)
  randomDouble                    = random

  -- generate a random value to select the material of the sphere
  (materialSelector, rngMaterial) = randomDouble rngMaterialSelector

  -- function to generate a Lambertian material
  getLambertian rng = Lambertian color
   where
    -- compute the color of the material by multiplying two random colors
    color              = toXYZ (color1 `mult` color2)
    (color1, rngColor) = randomColor rng
    (color2, _       ) = randomColor rngColor

  -- function to generate a Metallic material
  getMetallic rng = Metal fuzz color
   where
    (fuzz , rngColor) = random rng
    (color, _       ) = randomColor rngColor

  material | materialSelector < 0.8  = getLambertian rngMaterial
           | -- 80% of the time, generate a Lambertian material
             materialSelector < 0.95 = getMetallic rngMaterial
           | -- 15% of the time, generate a Metallic material
             otherwise               = Dieletric 1.5 -- otherwise, generate a Dielectric material with refractive index 1.5




randomScene :: Int -> Scene
randomScene rngSeed = Scene { antialiasing = 1
                            , objects      = defaultSpheres ++ randomSpheres
                            , camera       = cam
                            , rng          = mkStdGen 0
                            }
 where
  rngSpheres    = map mkStdGen $ randoms $ mkStdGen rngSeed
  randomSpheres = zipWith randomSphere
                          [ (a, b) | a <- [-11 .. 11], b <- [-11 .. 11] ]
                          rngSpheres
  defaultSpheres =
    [ Sphere { center   = fromXYZ (4, -1000, 0)
             , radius   = 1000
             , material = Lambertian (0.5, 0.5, 0.5)
             }
    , Sphere { center   = fromXYZ (0, 1, 0)
             , radius   = 1
             , material = Dieletric 1.5
             }
    , Sphere { center   = fromXYZ (-4, 1, 0)
             , radius   = 1
             , material = Lambertian (0.4, 0.2, 0.1)
             }
    , Sphere { center   = fromXYZ (4, 1, 0)
             , radius   = 1
             , material = Metal 0 (0.7, 0.6, 0.5)
             }
    ]

main :: IO ()
main = do
  let rngSeed             = 2 -- initialize the random number generator seed
  let s                   = randomScene rngSeed -- generate a random scene
  let nx                  = nPixelsHorizontal (camera s) -- compute the number of horizontal pixels in the image
  let ny                  = nPixelsVertical (camera s) -- compute the number of vertical pixels in the image
  let scenes = map (setRng s) (randoms $ mkStdGen rngSeed) -- generate a list of scenes with different random number generator states
  let pixelColors = zipWith getSceneColor scenes (screenPixels nx ny) -- compute the color of each pixel in the image
  -- compute the colors in parallel using 16-element chunks and fully reducing the results
  let parallelPixelColors = pixelColors `using` parListChunk 16 rdeepseq
  printPPMHeader s
  mapM_ printPixelColor parallelPixelColors -- print the color of each pixel in the image
