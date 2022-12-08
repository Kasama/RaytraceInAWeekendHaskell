module Color where

import           System.Random

-- Color from 0 to 255
type ColorInteger = (Integer, Integer, Integer)

-- Color from 0 to 1
type Color = (Double, Double, Double)

-- | Adds two `Color` values together component-wise.
add :: Color -> Color -> Color
(r1, g1, b1) `add` (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

-- | Multiplies two `Color` values together component-wise.
mult :: Color -> Color -> Color
(r1, g1, b1) `mult` (r2, g2, b2) = (r1 * r2, g1 * g2, b1 * b2)

-- | The maximum value that a pixel color component can have in a PPM image.
maxPixelColor = 255.99 :: Double

-- | Normalizes the given `Color` value so that its components are within the valid
-- range for a PPM image (i.e., between 0 and `maxPixelColor`).
normalizeColor :: Color -> ColorInteger
normalizeColor (r, g, b) = (r', g', b')
 where
  r'                   = normalizePixelColor' (sqrt r)
  g'                   = normalizePixelColor' (sqrt g)
  b'                   = normalizePixelColor' (sqrt b)
  normalizePixelColor' = normalizePixelColor maxPixelColor

-- | Normalizes the given pixel color component so that it is within the valid
-- range for a PPM image (i.e., between 0 and `maxPixelColor`).
normalizePixelColor :: Double -> Double -> Integer
normalizePixelColor maxPixelColor pixel = floor (maxPixelColor * pixel)

-- | Clamps the given value between the given minimum and maximum values.
clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

-- | Normalizes the given value so that it is within the range 0 to 1.
normalizeBetween0and1 :: Double -> Double
normalizeBetween0and1 n = clamp 0 1 $ 0.5 * (n + 1)

-- | Generate a random `Color` value using the given random number generator.
randomColor :: StdGen -> (Color, StdGen)
randomColor randomGen = (color, updatedRandomGen)
 where
  color                     = (red, green, blue)
  -- Generate the red component of the color.
  (red  , genForGreen     ) = randomR (0, 1) randomGen
  -- Generate the green component of the color.
  (green, genForBlue      ) = randomR (0, 1) genForGreen
  -- Generate the blue component of the color.
  (blue , updatedRandomGen) = randomR (0, 1) genForBlue



-- | Default Colors
white = (1, 1, 1) :: Color
red = (1, 0, 0) :: Color
green = (0, 1, 0) :: Color
blue = (0, 0, 1) :: Color
black = (0, 0, 0) :: Color
