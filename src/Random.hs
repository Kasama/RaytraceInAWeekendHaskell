{-# LANGUAGE TupleSections #-}

module Random where

import System.Random
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)
import Control.Monad.State


newtype Rand rng a = Rand { useRand :: rng -> (rng, a) }

instance Monad (Rand rng) where
  return a = Rand (, a)
  m >>= f = Rand $
    \rng -> case useRand m rng of
      (newRng, a) -> useRand (f a) newRng

instance Functor (Rand rng) where
  fmap = liftM

instance Applicative (Rand rng) where
  pure  = return
  (<*>) = ap

execRand :: Rand rng a -> rng -> a
execRand m x = snd (useRand m x)

----------------------------------------------------------






getNumber :: StdGen -> (StdGen, Double)
getNumber rng = tFlip $ randomR (0, 1) rng

tFlip :: (a, b) -> (b, a)
tFlip (a, b) = (b, a)

addNoise :: Integer -> Rand StdGen Double
addNoise a = return (fromInteger a)
  where
    gen = mkStdGen 0
    -- rand = return $ (fromInteger a) + dbl
    (newGen, dbl) = getNumber gen
