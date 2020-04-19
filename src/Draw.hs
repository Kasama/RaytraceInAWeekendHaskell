module Draw where

import Ray
import Vec3
import Shape
import Scene
import Camera
import Color
import System.Random
import Debug.Trace
import Text.Printf
import Data.List (minimumBy)
import Data.Vec3 hiding (origin, zipWith)

inf :: Double
inf = read "Infinity"

getCircularGradientBackgroundColor :: Color -> Color -> Ray -> Color
getCircularGradientBackgroundColor highColor lowColor ray = lerp lowColor highColor t
  where t = normalizeBetween0and1 yComponent
        (CVec3 _ yComponent _) = normalize $ direction ray

getBlueGradientBackground :: Ray -> Color
getBlueGradientBackground ray = (white .^ (1.0 - t)) <+> ((0.5, 0.7, 1.0) .^ t)
  where
    (_, y, _) = toXYZ $ direction ray
    t = (y + 1.0) * 0.5

getPinkBackgroundColor = getCircularGradientBackgroundColor (0.6, 0.1, 0.5) white

getBackgroundColor :: Ray -> Color
getBackgroundColor = getBlueGradientBackground

getColorForRay :: Scene -> Ray -> Integer -> StdGen -> (Color, StdGen)
getColorForRay scene ray tries rng
  | tries > 50 = (black, rng)
  | anyHits    = (nextColor `mult` materialColor, nrng)
  | otherwise  = (getBackgroundColor ray, rng)
  where
    (nextColor, nrng) = getColorForRay scene nextRay (tries + 1) nextRng
    hitRecords = filter didHit $ map (\o -> hit o ray 0.0001 inf) $ objects scene
    anyHits = any didHit hitRecords
    closestRecord = minimum hitRecords
    (nextRay, materialColor, didReflect, nextRng) = scatter ray closestRecord rng

getSceneColor' :: Scene -> UV -> StdGen -> (Color, StdGen)
getSceneColor' scene (u, v) = getColorForRay scene ray 0
  where
    ray = Ray {
      origin = position $ camera scene,
      direction = getRay (camera scene) (u, v)
    }

getSceneColor :: Scene -> (Integer, Integer) -> ColorInteger
getSceneColor scene (x, y) = normalizeColor $ toXYZ averageColor
  where
    nSamples = fromInteger $ antialiasing scene
    (genX, genY) = split (rng scene)
    uv a b = toUV (camera scene) (a, b)
    color sampleA sampleB = getSceneColor' scene (uv (fromInteger x + sampleA) (fromInteger y + sampleB))
    (colorSamples, _) = unzip $ take nSamples $ zipWith3 color (randoms genX) (randoms genY) (map mkStdGen (randoms (rng scene)))
    aggregated = foldr ((<+>) . fromXYZ) originVec colorSamples
    averageColor = aggregated .^ (1.0 / fromIntegral nSamples)
    (r, g, b) = toXYZ averageColor

-- PPM stuff
printPixelColor :: ColorInteger -> IO ()
printPixelColor (r, g, b) = putStrLn $ show r ++ " " ++ show g ++ " " ++ show b

printPPMHeader :: Scene -> IO ()
printPPMHeader scene =
  do putStrLn "P3";
     putStrLn $ show (nPixelsHorizontal (camera scene)) ++ " " ++ show (nPixelsVertical (camera scene));
     print $ floor maxPixelColor
