module Scratch where

returnTheFunc :: Int -> (Int -> Int -> Int) -> (Int -> Int -> Int)
returnTheFunc _ f = f

-- >>> (returnTheFunc 1 (+)) 2 3
-- 5
