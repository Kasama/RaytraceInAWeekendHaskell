module Scene where

import Data.Vec3
import System.Random
import Shape
import Camera

-- | Data structure representing a 3D scene.
data Scene = Scene {
  -- | The number of samples to take when rendering an image with antialiasing.
  antialiasing :: Integer,
  -- | The objects that make up the scene.
  objects :: [Shape],
  -- | The camera used to view the scene.
  camera :: Camera,
  -- | The random number generator used to generate random numbers.
  rng :: StdGen
}

-- | Set the random number generator used in the given scene.
setRng :: Scene -> Int -> Scene
setRng s rngSeed = Scene
                     (antialiasing s)
                     (objects s)
                     (camera s)
                     (mkStdGen rngSeed)
