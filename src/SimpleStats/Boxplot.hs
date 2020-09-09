{-# LANGUAGE ScopedTypeVariables #-}

module SimpleStats.Boxplot
    (
      calc
    )
where

import           Data.List (sort)


calc :: (Ord a, Num a) => [a] -> [a]
calc [] = []
calc [a] = replicate 5 a
calc tms =
    let s_tms = sort tms
        -- len = length s_tms
        min_tm = head s_tms
        max_tm = last s_tms
        q1 = quartile s_tms 0.25
        q2 = quartile s_tms 0.50  -- median
        q3 = quartile s_tms 0.75
    in [min_tm, q1, q2, q3, max_tm]
  where
    quartile :: (Num a, Ord a) => [a] -> Double -> a
    quartile [] _ = -0
    quartile ls frac
        | frac > 0 && frac <= 1.0 =
            let len :: Double = fromIntegral (length ls)
                idx = round (len * frac)
            in ls !! idx
        | otherwise = -0
