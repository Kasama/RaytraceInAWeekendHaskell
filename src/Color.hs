module Color where

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
  where r' = normalizePixelColor' r
        g' = normalizePixelColor' g
        b' = normalizePixelColor' b
        normalizePixelColor' = normalizePixelColor maxPixelColor

normalizePixelColor :: Double -> Double -> Integer
normalizePixelColor maxPixelColor pixel = floor (maxPixelColor * pixel)

normalizeBetween0and1 :: Double -> Double
normalizeBetween0and1 n = 0.5 * (n + 1)

-- Default Colors
white = (1, 1, 1) :: Color
red   = (1, 0, 0) :: Color
green = (0, 1, 0) :: Color
blue  = (0, 0, 1) :: Color
black = (0, 0, 0) :: Color