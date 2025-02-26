module PhoneBook where

import Text.Regex.TDFA ((=~))

-- | A simple phone book entry with a name and phone number
data Entry = Entry {name :: String, phone :: String}

instance Show Entry where
  show :: Entry -> String
  show (Entry name phone) = " Name: " ++ name ++ ", Number: " ++ phone

type PhoneBook = [Entry]

-- | Check if an entry already exists in the phone book
--
-- >>> entryExists [Entry "Mike" "123"] (Entry "John" "456")
-- False
entryExists :: PhoneBook -> Entry -> Bool
entryExists pb e = any (\x -> name x == name e) pb

-- | Check if a phone number is valid
--
-- >>> isValidPhoneNumber "123-123-1234"
-- True
-- >>> isValidPhoneNumber "123"
-- False
isValidPhoneNumber :: String -> Bool
isValidPhoneNumber number = number =~ "^[0-9]{3}-[0-9]{3}-[0-9]{4}$"

-- | Add an entry to the phone book
-- Returns an error message if the phone number is invalid or the entry already exists
--
-- >>> addEntry [] (Entry "John" "123-123-1234")
-- Right [ Name: John, Number: 123-123-1234]
-- >>> addEntry [Entry "John" "123-123-4567"] (Entry "John" "555-555-5555")
-- Left "Entry already exists"
-- >>> addEntry [Entry "John" "123"] (Entry "Jane" "456")
-- Left "Invalid phone number"
addEntry :: PhoneBook -> Entry -> Either String PhoneBook
addEntry pb e
  | not validNumber = Left "Invalid phone number"
  | entryExists pb e = Left "Entry already exists"
  | otherwise = Right $ e : pb
  where
    validNumber = isValidPhoneNumber $ phone e

-- | Update an entry in the phone book
-- Returns an error message if the phone number is invalid or the entry does not exist
--
-- >>> updateNumber [Entry "John" "123-456-7890"] (Entry "John" "555-555-5555")
-- Right [ Name: John, Number: 555-555-5555]
-- >>> updateNumber [Entry "John" "123-456-7890"] (Entry "Jane" "555-555-5555")
-- Left "Entry not found"
updateNumber :: PhoneBook -> Entry -> Either String PhoneBook
updateNumber pb e
  | not validNumber = Left "Invalid phone number"
  | not $ entryExists pb e = Left "Entry not found"
  | otherwise = Right $ map update pb
  where
    validNumber = isValidPhoneNumber $ phone e
    update x = if name x == name e then e else x
