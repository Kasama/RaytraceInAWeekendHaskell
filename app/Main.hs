module Main where

import Draw ( getSceneColor
            , printPPMHeader
            , printPixelColor
            )
import Scene
import Shape
import Data.Vec3

screenPixels :: [(Integer, Integer)]
screenPixels = [ (x, y)
               | y <- [ny-1,ny-2..0]
               , x <- [0..nx-1]
               ]

shapes = [ Sphere { center = fromXYZ (0, 0, -1), radius = 0.5 }
         , Sphere { center = fromXYZ (0, -100.5, -1), radius = 100 }
         ]

scene = Scene { aspectRatioH = 2
              , aspectRatioV = 1
              , scale        = 100
              , objects      = shapes
              }

nx = nPixelsHorizontal scene
ny = nPixelsVertical scene

getSceneColor' = getSceneColor scene
printPPMHeader' = printPPMHeader scene

main :: IO ()
main = do printPPMHeader';
          mapM_ (printPixelColor . getSceneColor') screenPixels
