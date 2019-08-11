module Draw where

import Ray
import Vec3
import Data.Vec3

type Color = (Integer, Integer, Integer)
type Color01 = (Double, Double, Double)
type UV = (Double, Double)

maxPixelColor = 255.99 :: Double

normalizeColor :: Color01 -> Color
normalizeColor (r, g, b) = (r', g', b')
  where
    r' = normalizePixelColor' r
    g' = normalizePixelColor' g
    b' = normalizePixelColor' b
    normalizePixelColor' = normalizePixelColor maxPixelColor

normalizePixelColor :: Double -> Double -> Integer
normalizePixelColor maxPixelColor pixel = floor $ maxPixelColor * pixel

getBackgroundColor' :: Ray a => Color01 -> Color01 -> a -> Color01
getBackgroundColor' highColor lowColor ray = lerp lowColor highColor t
  where
    t = normalizeBetween0and1 $ y normalizedDirection
    normalizedDirection = normalize $ direction ray
    normalizeBetween0and1 n = 0.5 * (n + 1)
    y (CVec3 _ y' _) = y'

getBackgroundColor :: Ray a => a -> Color01
getBackgroundColor = getBackgroundColor' (0.6, 0.1, 0.5) (1, 1, 1)

toUV :: Integer -> Integer -> (Integer, Integer) -> UV
toUV nPixelsHorizontal nPixelsVertical (x, y) = (u, v)
  where
    u = fromInteger x / fromInteger nPixelsHorizontal
    v = fromInteger y / fromInteger nPixelsVertical

getSceneColor' :: UV -> Color01
getSceneColor' (u, v) = getBackgroundColor ray
  where
    ray = VisionRay {
      rayOrigin = fromXYZ (0, 0, 0),
      rayDirection = direction
    }
    direction = lowerLeftCorner <+> (horizontal .^ u) <+> (vertical .^ v)
    lowerLeftCorner = fromXYZ (-2, -1, -1)
    horizontal = fromXYZ (4, 0, 0)
    vertical = fromXYZ (0, 2, 0)

getSceneColor :: Integer -> Integer -> (Integer, Integer) -> Color
getSceneColor nx ny xy = normalizeColor $ getSceneColor' uv
  where
    uv = toUV nx ny xy

-- PPM stuff
printPixelColor :: Color -> IO ()
printPixelColor (r, g, b) = putStrLn $ show r ++ " " ++ show g ++ " " ++ show b

printPPMHeader :: Integer -> Integer -> IO ()
printPPMHeader nPixelsHorizontal nPixelsVertical = do putStrLn "P3";
                                                      putStrLn $ show nPixelsHorizontal ++ " " ++ show nPixelsVertical;
                                                      putStrLn "255"
