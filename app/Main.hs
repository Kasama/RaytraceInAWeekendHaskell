module Main where

import Scene
import Shape
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

scene :: Int -> Scene
scene seed = Scene { aspectRatioH = 2
                   , aspectRatioV = 1
                   , scale        = 200
                   , antialiasing = 100
                   , objects      = shapes
                   , rng          = randoms $ mkStdGen seed
                   }

main :: IO ()
main = do
          let rngSeed = 0
          let s = scene rngSeed
          let nx = nPixelsHorizontal s
          let ny = nPixelsVertical s
          let getSceneColor' = getSceneColor s
          printPPMHeader s;
          mapM_ (printPixelColor . getSceneColor') $ screenPixels nx ny
