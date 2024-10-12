module Calculator where

data DTO = List [Int] | Num Int deriving (Show)

data Operation = Add | Subtract | Multiply | Divide | OpList [Operation]

calculate :: DTO -> DTO -> Operation -> Either String DTO
calculate (Num x) (Num y) Add = Right $ Num (x + y)
calculate (Num x) (Num y) Subtract = Right $ Num (x - y)
calculate (Num x) (Num y) Multiply = Right $ Num (x * y)
calculate (Num x) (Num y) Divide
  | y == 0 = Left "Cannot divide by zero"
  | otherwise = Right $ Num (x `div` y)
calculate (List xs) (List ys) (OpList ops) = List <$> traverse calculatePair (zip3 xs ys ops)
calculate _ _ _ = Left "Invalid operation"

calculatePair :: (Int, Int, Operation) -> Either String Int
calculatePair (a, b, op) = case calculate (Num a) (Num b) op of
  Right (Num result) -> Right result
  Right (List _) -> Left "Unexpected List result"
  Left err -> Left err

-- >>> List <$> traverse calculatePair [(1, 2, Add), (3, 4, Subtract), (5, 6, Multiply), (7, 1, Divide)]
-- Right (List [3,-1,30,7])
