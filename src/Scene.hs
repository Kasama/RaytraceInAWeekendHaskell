module Scene where

import Data.Vec3
import System.Random
import Shape
import Camera

data Scene = Scene { antialiasing :: Integer
                   , objects :: [Shape]
                   , camera :: Camera
                   , rng :: StdGen
                   }

setRng :: Scene -> Int -> Scene
setRng s rngSeed = Scene
                     (antialiasing s)
                     (objects s)
                     (camera s)
                     (mkStdGen rngSeed)
