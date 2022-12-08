module Draw where

import Ray
import Vec3
import Shape
import Scene
import Camera
import Color
import System.Random
import Text.Printf
import Data.Maybe
import Data.List (minimumBy)
import Data.Vec3 hiding (origin, zipWith)

inf :: Double
inf = read "Infinity"

getCircularGradientBackgroundColor :: Color -> Color -> Ray -> Color
getCircularGradientBackgroundColor highColor lowColor ray = lerp lowColor highColor t
  where t = normalizeBetween0and1 yComponent
        (CVec3 _ yComponent _) = normalize $ direction ray

getBlueGradientBackground :: Ray -> Color
getBlueGradientBackground ray = lerp white (0.5, 0.7, 1.0) t
  where
    (_, y, _) = toXYZ $ direction ray
    t = normalizeBetween0and1 y

getPinkBackgroundColor = getCircularGradientBackgroundColor (0.6, 0.1, 0.5) white

getBackgroundColor :: Ray -> Color
getBackgroundColor = getBlueGradientBackground

-- | Get the color of an object in the scene by tracing a ray from the
-- camera through the object and finding the first object it hits.
getColorForRay :: Scene -> Ray -> Integer -> StdGen -> (Color, StdGen)
-- If the ray has been traced too many times, return a default color.
-- Otherwise, if the ray hits an object, return the color of that object
-- multiplied by the material color of the object. Otherwise, return the
-- background color.
getColorForRay scene ray traceDepth randomGen
  | traceDepth > 50 = (black, randomGen)
  | anyHits         = (nextColor `mult` materialColor, updatedRandomGen)
  | otherwise       = (getBackgroundColor ray, randomGen)
  where
    -- Recursively trace the next ray and get its color.
    (nextColor, updatedRandomGen) = getColorForRay scene nextRay (traceDepth + 1) nextRandomGen
    -- Get the hit records for all objects in the scene.
    hitRecords = mapMaybe (\o -> hit o ray 0.0001 inf) $ objects scene
    -- Check if the ray hit any objects.
    anyHits = not . null $ hitRecords
    -- Get the hit record for the closest object hit by the ray.
    closestRecord = minimum hitRecords
    -- Scatter the ray and get the material color of the object.
    (nextRay, materialColor, nextRandomGen) = scatter ray closestRecord randomGen

-- | Get the color of a pixel in a scene.
getSceneColor' :: Scene -> UV -> StdGen -> (Color, StdGen)
-- Compute the color of the pixel by tracing a ray from the camera
-- through the pixel and finding the first object it hits.
getSceneColor' scene uv rng = getColorForRay scene ray 0 nextRng
  where
    -- Get the camera for the scene.
    cam = camera scene
    -- Generate a random ray through the pixel.
    (ray, nextRng) = getRay cam cam uv rng

-- | Get the color of a pixel in a scene by sampling multiple rays per pixel
getSceneColor :: Scene -> (Integer, Integer) -> ColorInteger
-- Compute the color of the pixel by averaging the colors of
-- multiple samples taken within the pixel.
getSceneColor scene (x, y) = normalizeColor $ toXYZ averageColor
  where
    -- The number of samples to take within the pixel.
    nSamples = fromInteger $ antialiasing scene
    -- Split the random number generator to generate random
    -- offsets for the sample positions.
    (genX, genY) = split (rng scene)
    -- Convert pixel coordinates to UV coordinates.
    uv a b = toUV (camera scene) (a, b)
    -- Function to get the color of a single sample in the pixel.
    color sampleA sampleB = getSceneColor' scene (uv (fromInteger x + sampleA) (fromInteger y + sampleB))
    -- Get the colors of all samples in the pixel.
    (colorSamples, _) = unzip $ take nSamples $ zipWith3 color (randoms genX) (randoms genY) (map mkStdGen (randoms (rng scene)))
    -- Aggregate the sample colors by summing them together.
    aggregated = foldr ((<+>) . fromXYZ) originVec colorSamples
    -- Compute the average color of the samples.
    averageColor = aggregated .^ (1.0 / fromIntegral nSamples)
    -- Convert the average color to an rgb integer triple.
    (r, g, b) = toXYZ averageColor


-- PPM stuff
-- | Print the color of a single pixel in a PPM image.
printPixelColor :: ColorInteger -> IO ()
-- Print the red, green, and blue components of the pixel
-- color as integers separated by spaces.
printPixelColor (r, g, b) = putStrLn $ show r ++ " " ++ show g ++ " " ++ show b

-- | Print the header of a PPM image file.
printPPMHeader :: Scene -> IO ()
-- Print the magic number, image dimensions, and maximum
-- color value to standard output.
printPPMHeader scene =
  do putStrLn "P3";
     putStrLn $ show (nPixelsHorizontal (camera scene)) ++ " " ++ show (nPixelsVertical (camera scene));
     print $ floor maxPixelColor

