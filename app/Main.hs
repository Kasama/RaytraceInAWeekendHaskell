module Main where

import Draw ( getSceneColor
            , printPPMHeader
            , printPixelColor
            )

nPixelsHorizontal = 200 :: Integer
nPixelsVertical   = 100 :: Integer

screenPixels :: [(Integer, Integer)]
screenPixels = [ (x, y)
               | y <- [nPixelsVertical-1,nPixelsVertical-2..0]
               , x <- [0..nPixelsHorizontal-1]
               ]

getSceneColor' = getSceneColor nPixelsHorizontal nPixelsVertical
printPPMHeader' = printPPMHeader nPixelsHorizontal nPixelsVertical

main :: IO ()
main = do printPPMHeader';
          mapM_ (printPixelColor . getSceneColor') screenPixels
