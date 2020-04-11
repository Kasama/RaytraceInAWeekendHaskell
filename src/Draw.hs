module Draw where

import Ray
import Vec3
import Shape
import Scene
import Camera
import System.Random
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

getCircularGradientBackgroundColor :: Color01 -> Color01 -> Ray -> Color01
getCircularGradientBackgroundColor highColor lowColor ray = lerp lowColor highColor t
  where t = normalizeBetween0and1 yComponent
        (CVec3 _ yComponent _) = normalize $ direction ray

getBackgroundColor :: Ray -> Color01
getBackgroundColor = getCircularGradientBackgroundColor (0.6, 0.1, 0.5) (1, 1, 1)

getSceneColor' :: Scene -> UV -> Color01
getSceneColor' scene (u, v)
  | anyHits   = normalColor
  | otherwise = getBackgroundColor ray
  where
    ray = Ray {
      origin = position $ camera scene,
      direction = getRay (camera scene) (u, v)
    }
    hitRecords = filter didHit $ map (\o -> hit o ray 0 inf) $ objects scene
    anyHits = any didHit hitRecords
    closestRecord = minimum hitRecords
    normalColor = toXYZ $ vmap (1 +) (normal closestRecord) .^ 0.5

getSceneColor :: Scene -> (Integer, Integer) -> Color
getSceneColor scene (x, y) = normalizeColor $ toXYZ averageColor
  where
    nSamples = fromInteger $ antialiasing scene
    (genX, genY) = split (rng scene)
    uv a b = toUV (camera scene) (a, b)
    color sampleA sampleB = getSceneColor' scene (uv (fromInteger x + sampleA) (fromInteger y + sampleB))
    colorSamples = take nSamples $ zipWith color (randoms genX) (randoms genY)
    aggregated = foldr ((<+>) . fromXYZ) originVec colorSamples
    averageColor = aggregated .^ (1.0 / fromIntegral nSamples)

-- PPM stuff
printPixelColor :: Color -> IO ()
printPixelColor (r, g, b) = putStrLn $ show r ++ " " ++ show g ++ " " ++ show b

printPPMHeader :: Scene -> IO ()
printPPMHeader scene =
  do putStrLn "P3";
     putStrLn $ show (nPixelsHorizontal (camera scene)) ++ " " ++ show (nPixelsVertical (camera scene));
     print $ floor maxPixelColor
