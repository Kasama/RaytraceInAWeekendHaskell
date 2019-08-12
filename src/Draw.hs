module Draw where

import Ray
import Vec3
import Shape
import Data.Vec3 hiding (origin)

inf :: Double
inf = read "Infinity"

type Color = (Integer, Integer, Integer)
type Color01 = (Double, Double, Double)
type UV = (Double, Double)

maxPixelColor = 255.99 :: Double

normalizeColor :: Color01 -> Color
normalizeColor (r, g, b) = (r', g', b')
  where r' = normalizePixelColor' r
        g' = normalizePixelColor' g
        b' = normalizePixelColor' b
        normalizePixelColor' = normalizePixelColor maxPixelColor

normalizePixelColor :: Double -> Double -> Integer
normalizePixelColor maxPixelColor pixel = floor $ maxPixelColor * pixel

normalizeBetween0and1 :: Double -> Double
normalizeBetween0and1 n = 0.5 * (n + 1)

getBackgroundColor' :: Color01 -> Color01 -> Ray -> Color01
getBackgroundColor' highColor lowColor ray = lerp lowColor highColor t
  where t = normalizeBetween0and1 yComponent
        (CVec3 _ yComponent _) = normalize $ direction ray

getBackgroundColor :: Ray -> Color01
getBackgroundColor = getBackgroundColor' (0.6, 0.1, 0.5) (1, 1, 1)

toUV :: Integer -> Integer -> (Integer, Integer) -> UV
toUV nPixelsHorizontal nPixelsVertical (x, y) = (u, v)
  where u = fromInteger x / fromInteger nPixelsHorizontal
        v = fromInteger y / fromInteger nPixelsVertical

getSceneColor' :: UV -> Color01
getSceneColor' (u, v)
  | didHit hitRecord = normalColor
  | otherwise        = getBackgroundColor ray
  where
    ray = Ray {
      origin = fromXYZ (0, 0, 0),
      direction = lowerLeftCorner <+> (horizontal .^ u) <+> (vertical .^ v)
    }
    sphere = Sphere {
      center = fromXYZ (0, 0, -1),
      radius = 0.5
    }
    lowerLeftCorner = fromXYZ (-2, -1, -1)
    horizontal = fromXYZ (4, 0, 0)
    vertical = fromXYZ (0, 2, 0)
    hitRecord = hit sphere ray 0 inf
    normalColor = toXYZ $ vmap (1 +) (normal hitRecord) .^ 0.5

getSceneColor :: Integer -> Integer -> (Integer, Integer) -> Color
getSceneColor nx ny xy = normalizeColor $ getSceneColor' uv
  where uv = toUV nx ny xy

-- PPM stuff
printPixelColor :: Color -> IO ()
printPixelColor (r, g, b) = putStrLn $ show r ++ " " ++ show g ++ " " ++ show b

printPPMHeader :: Integer -> Integer -> IO ()
printPPMHeader nPixelsHorizontal nPixelsVertical = do putStrLn "P3";
                                                      putStrLn $ show nPixelsHorizontal ++ " " ++ show nPixelsVertical;
                                                      putStrLn "255"
