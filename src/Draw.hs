module Draw where

import Ray
import Vec3
import Shape
import Scene
import Data.List (minimumBy)
import Data.Vec3 hiding (origin, zipWith)

inf :: Double
inf = read "Infinity"

-- Color from 0 to 255
type Color = (Integer, Integer, Integer)

-- Color from 0 to 1
type Color01 = (Double, Double, Double)

add :: Color01 -> Color01 -> Color01
(r1, g1, b1) `add` (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

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

getGradientBackgroundColor :: Color01 -> Color01 -> Ray -> Color01
getGradientBackgroundColor highColor lowColor ray = lerp lowColor highColor t
  where t = normalizeBetween0and1 yComponent
        (CVec3 _ yComponent _) = normalize $ direction ray

getBackgroundColor :: Ray -> Color01
getBackgroundColor = getGradientBackgroundColor (0.6, 0.1, 0.5) (1, 1, 1)

getSceneColor' :: Scene -> UV -> Color01
getSceneColor' s (u, v)
  | anyHits   = normalColor
  | otherwise = getBackgroundColor ray
  where
    ray = Ray {
      origin = originVec,
      direction = lowerLeftCorner s <+> (horizontal s .^ u) <+> (vertical s .^ v)
    }
    hitRecords = filter didHit $ map (\o -> hit o ray 0 inf) $ objects s
    anyHits = any didHit hitRecords
    closestRecord = minimum hitRecords
    normalColor = toXYZ $ vmap (1 +) (normal closestRecord) .^ 0.5

getSceneColor :: Scene -> (Integer, Integer) -> Color
getSceneColor scene (x, y) = normalizeColor $ getSceneColor' scene (toUV scene (fromInteger x, fromInteger y))

-- PPM stuff
printPixelColor :: Color -> IO ()
printPixelColor (r, g, b) = putStrLn $ show r ++ " " ++ show g ++ " " ++ show b

printPPMHeader :: Scene -> IO ()
printPPMHeader scene =
  do putStrLn "P3";
     putStrLn $ show (nPixelsHorizontal scene) ++ " " ++ show (nPixelsVertical scene);
     print $ floor maxPixelColor
