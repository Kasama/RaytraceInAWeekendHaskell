module Draw where

type Color = (Integer, Integer, Integer)

normalizePixelColor :: Double -> Double -> Integer
normalizePixelColor maxPixelColor pixel = floor $ maxPixelColor * pixel

getSceneColor :: Double -> Double -> Double -> (Double, Double) -> Color
getSceneColor nPixelsHorizontal nPixelsVertical maxPixelColor (x, y) = (r, g, b)
  where
    r = normalizePixelColor' $ x / nPixelsHorizontal
    g = normalizePixelColor' $ y / nPixelsVertical
    b = normalizePixelColor' 0.2
    normalizePixelColor' = normalizePixelColor maxPixelColor

printPixelColor :: Color -> IO ()
printPixelColor (r, g, b) = putStrLn $ show r ++ " " ++ show g ++ " " ++ show b

printPPMHeader :: Double -> Double -> IO ()
printPPMHeader nPixelsHorizontal nPixelsVertical = do putStrLn "P3";
                                                      putStrLn $ show nPixelsHorizontal ++ " " ++ show nPixelsVertical;
                                                      putStrLn "255"
