module Main where

import Calculator
import qualified Colors as C

main :: IO ()
main = do
  let add = calculate (Num 1) (Num 2) Add
  let sub = calculate (Num 1) (Num 2) Subtract
  let l = calculate (List [1, 2, 3]) (List [4, 5, 6]) (OpList [Add, Subtract, Multiply])
  putStrLn $ "1 + 2 = " ++ show add
  putStrLn $ "1 - 2 = " ++ show sub
  putStrLn $ "[1, 2, 3] [4, 5, 6] [Add, Subtract, Multiply]= " ++ show l
  putStrLn $ "1 / 0 = " ++ show (calculate (Num 1) (Num 0) Divide)
  mapM_ C.printFavoriteColor C.persons

-- Prints
-- The hex code of Bob's favorite color is: #FF0000
-- The hex code of Fran's favorite color is: #0000FF
