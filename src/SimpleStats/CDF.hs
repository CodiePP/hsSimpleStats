{-# LANGUAGE ScopedTypeVariables #-}

module SimpleStats.CDF
    (
      calc
    )
where

import           Data.List (sort)
import           GHC.Float (int2Double)


calc :: (Ord a, Integral a) => [a] -> [(a, Double)]
calc [] = []
calc tms =
    let s_tms = sort tms
        min_tm :: Double = fromIntegral (head s_tms)
        max_tm :: Double = fromIntegral (last s_tms)
    in map (calc_fraction min_tm max_tm s_tms) [1::Double .. 100]
  where
    calc_fraction min_tm max_tm ls q =
        let v = round $ (max_tm - min_tm) * q / 100.0 + min_tm
        in (v, fraction ls v)
    -- | count elements in sorted list smaller or equal than value
    --   TODO: speedup if consecutive calls only count in remaining list
    fraction :: Ord a => [a] -> a -> Double
    fraction ls val =
        let len = length ls
            count = counting ls 0
        in int2Double count / int2Double len
      where
        counting [] c = c
        counting (v : r) c
            | v <= val = counting r (c + 1)
            | otherwise = c

