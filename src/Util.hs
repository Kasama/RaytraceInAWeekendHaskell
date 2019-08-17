module Util where

evens (x:xs) = x : odds xs
evens _      = []

odds (_:xs) = evens xs
odds _      = []

cantor a b = ((a + b) * (a + b + 1) `div` 2) + b
