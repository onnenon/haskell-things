module PhoneBook where

import Text.Regex.TDFA ((=~))

data Entry = Entry {name :: String, phone :: String}

instance Show Entry where
  show :: Entry -> String
  show (Entry name phone) = " Name: " ++ name ++ ", Number: " ++ phone

type PhoneBook = [Entry]

entryExists :: PhoneBook -> Entry -> Bool
entryExists pb e = any (\x -> name x == name e) pb

isValidPhoneNumber :: String -> Bool
isValidPhoneNumber number = number =~ "^[0-9]{3}-[0-9]{3}-[0-9]{4}$"

addEntry :: PhoneBook -> Entry -> Either String PhoneBook
addEntry pb e
  | not $ isValidPhoneNumber $ phone e = Left "Invalid phone number"
  | entryExists pb e = Left "Entry already exists"
  | otherwise = Right $ e : pb

updateNumber :: PhoneBook -> Entry -> Either String PhoneBook
updateNumber pb e
  | not validNumber = Left "Invalid phone number"
  | not $ entryExists pb e = Left "Entry not found"
  | otherwise = Right $ map update pb
  where
    validNumber = isValidPhoneNumber $ phone e
    update x = if name x == name e then e else x

-- >>> addEntry [] (Entry "John" "123-123-1234")
-- Right [ Name: John, Number: 123-123-1234]

-- >>> addEntry [Entry "John" "123"] (Entry "John" "456")
-- Left "Invalid phone number"

-- >>> updateNumber [Entry "John" "123"] (Entry "John" "456")
-- Left "Invalid phone number"

-- >>> updateNumber [Entry "John" "123"] (Entry "Jane" "456")
-- Left "Invalid phone number"
