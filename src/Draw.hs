module Draw where

type Color = (Integer, Integer, Integer)

maxPixelColor = 255.99 :: Double

normalizePixelColor :: Double -> Double -> Integer
normalizePixelColor maxPixelColor pixel = floor $ maxPixelColor * pixel

getSceneColor :: Integer -> Integer -> (Integer, Integer) -> Color
getSceneColor nPixelsHorizontal nPixelsVertical (x, y) = (r, g, b)
  where
    r = normalizePixelColor' $ fromInteger x / fromInteger nPixelsHorizontal
    g = normalizePixelColor' $ fromInteger y / fromInteger nPixelsVertical
    b = normalizePixelColor' 0.2
    normalizePixelColor' = normalizePixelColor maxPixelColor

printPixelColor :: Color -> IO ()
printPixelColor (r, g, b) = putStrLn $ show r ++ " " ++ show g ++ " " ++ show b

printPPMHeader :: Integer -> Integer -> IO ()
printPPMHeader nPixelsHorizontal nPixelsVertical = do putStrLn "P3";
                                                      putStrLn $ show nPixelsHorizontal ++ " " ++ show nPixelsVertical;
                                                      putStrLn "255"
