{-# LANGUAGE ScopedTypeVariables #-}
module Test
(
myFunction
) where

import Data.List

main = print $ myFunction [1,1,3,2]

-- | Showcase fir ScopedTypeVariables
myFunction :: forall a. Ord a => [a] -> [(a, a)]
myFunction inputList = zip sortedList nubbedList
    where sortedList :: [a]
          sortedList = sort inputList
          nubbedList :: [a]
          nubbedList = nub inputList

-- | Showcase for LiberalTypeSynonyms
