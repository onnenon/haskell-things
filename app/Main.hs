module Main where

-- import Calculator
-- import Colors qualified as C
import PhoneBook

main :: IO ()
main = do
  let pb =
        [ Entry "John" "123-456-7890",
          Entry "Jane" "456-123-4567",
          Entry "Bob" "555-555-5555",
          Entry "Fran" "123-456-7890"
        ]
  pb' <- promptForEntry pb
  putStrLn $ "Phone book: " ++ show pb'
  pb'' <- promptForUpdate pb'
  putStrLn $ "Phone book: " ++ show pb''

promptForEntry :: PhoneBook -> IO PhoneBook
promptForEntry pb = do
  putStrLn "Enter a name:"
  name <- getLine
  putStrLn "Enter a phone number:"
  phone <- getLine
  let entry = Entry name phone
  case addEntry pb entry of
    Right pb' -> do
      putStrLn $ "Added entry: " ++ show entry
      return pb'
    Left err -> do
      putStrLn err
      promptForEntry pb

promptForUpdate :: PhoneBook -> IO PhoneBook
promptForUpdate pb = do
  putStrLn "Enter an existing name to update:"
  name <- getLine
  putStrLn "Enter a new phone number:"
  phone <- getLine
  let entry = Entry name phone
  case updateNumber pb entry of
    Right pb' -> do
      putStrLn $ "Updated entry: " ++ show entry
      return pb'
    Left err -> do
      putStrLn err
      promptForUpdate pb

-- let add = calculate (Num 1) (Num 2) Add
-- let sub = calculate (Num 1) (Num 2) Subtract
-- let l = calculate (List [1, 2, 3]) (List [4, 5, 6]) (OpList [Add, Subtract, Multiply])
-- putStrLn $ "1 + 2 = " ++ show add
-- putStrLn $ "1 - 2 = " ++ show sub
-- putStrLn $ "[1, 2, 3] [4, 5, 6] [Add, Subtract, Multiply]= " ++ show l
-- putStrLn $ "1 / 0 = " ++ show (calculate (Num 1) (Num 0) Divide)
-- mapM_ C.printFavoriteColor C.persons

-- Prints
-- The hex code of Bob's favorite color is: #FF0000
-- The hex code of Fran's favorite color is: #0000FF
