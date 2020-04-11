module Main where

import Scene
import Shape
import Camera
import Data.Vec3
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

shapes = [ Sphere { center = fromXYZ (0, 0, -1), radius = 0.5 }
         , Sphere { center = fromXYZ (0, -100.5, -1), radius = 100 }
         ]

cam :: Camera
cam = getCamera (fromXYZ (5, 0, 10))
                (2, 1) -- Aspect Ratio 2x1
                200

scene :: Scene
scene = Scene { antialiasing = 4
              , objects      = shapes
              , camera       = cam
              , rng          = mkStdGen 0
              }

renderPixel :: Scene -> (Integer, Integer) -> IO ()
renderPixel scene = printPixelColor . getSceneColor scene

main :: IO ()
main = do
          let rngSeed = 0
          let s = scene
          let nx = nPixelsHorizontal (camera s)
          let ny = nPixelsVertical (camera s)
          let getSceneColor' = getSceneColor s
          let scenes = map (setRng s) (randoms $ mkStdGen rngSeed)
          printPPMHeader s;
          mapM_ (uncurry renderPixel) $ zip scenes (screenPixels nx ny)
