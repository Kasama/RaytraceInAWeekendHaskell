module Color where

import System.Random

-- Color from 0 to 255
type ColorInteger = (Integer, Integer, Integer)

-- Color from 0 to 1
type Color = (Double, Double, Double)

add :: Color -> Color -> Color
(r1, g1, b1) `add` (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

mult :: Color -> Color -> Color
(r1, g1, b1) `mult` (r2, g2, b2) = (r1 * r2, g1 * g2, b1 * b2)

maxPixelColor = 255.99 :: Double

normalizeColor :: Color -> ColorInteger
normalizeColor (r, g, b) = (r', g', b')
  where r' = normalizePixelColor' (sqrt r)
        g' = normalizePixelColor' (sqrt g)
        b' = normalizePixelColor' (sqrt b)
        normalizePixelColor' = normalizePixelColor maxPixelColor

normalizePixelColor :: Double -> Double -> Integer
normalizePixelColor maxPixelColor pixel = floor (maxPixelColor * pixel)

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

normalizeBetween0and1 :: Double -> Double
normalizeBetween0and1 n = clamp 0 1 $ 0.5 * (n + 1)

randomColor :: StdGen -> (Color, StdGen)
randomColor gen = (color, nextGen)
  where
    color = (x, y, z)
    (x, genY) = randomR (0, 1) gen
    (y, genZ) = randomR (0, 1) genY
    (z, nextGen) = randomR (0, 1) genZ

-- Default Colors
white = (1, 1, 1) :: Color
red   = (1, 0, 0) :: Color
green = (0, 1, 0) :: Color
blue  = (0, 0, 1) :: Color
black = (0, 0, 0) :: Color
