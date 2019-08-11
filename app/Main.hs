module Main where

import Draw ( getSceneColor
            , printPPMHeader
            , printPixelColor
            )

nPixelsHorizontal = 200 :: Double
nPixelsVertical = 100 :: Double

maxPixelColor = 255.99 :: Double

screenPixels = [ (x, y)
               | y <- [nPixelsVertical-1,nPixelsVertical-2..0]
               , x <- [0..nPixelsHorizontal-1]
               ]

getSceneColor' = getSceneColor nPixelsHorizontal nPixelsVertical maxPixelColor
printPPMHeader' = printPPMHeader nPixelsHorizontal nPixelsVertical

main :: IO ()
main = do printPPMHeader';
          mapM_ (printPixelColor . getSceneColor') screenPixels
