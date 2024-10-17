module PhoneBook where

data Entry = Entry {name :: String, phone :: String}

instance Show Entry where
  show :: Entry -> String
  show (Entry name phone) = " Name: " ++ name ++ ", Number: " ++ phone

type PhoneBook = [Entry]

entryExists :: PhoneBook -> Entry -> Bool
entryExists pb e = any (\x -> name x == name e) pb

addEntry :: PhoneBook -> Entry -> Either String PhoneBook
addEntry pb e =
  if entryExists pb e
    then Left "Entry already exists"
    else Right $ e : pb

updateNumber :: PhoneBook -> Entry -> Either String PhoneBook
updateNumber pb e =
  if entryExists pb e
    then Right $ map update pb
    else Left "Entry not found"
  where
    update x = if name x == name e then e else x

-- >>> addEntry [] (Entry "John" "123")
-- Right [Name: John, Number: 123]

-- >>> addEntry [Entry "John" "123"] (Entry "John" "456")
-- Left "Entry already exists"

-- >>> updateNumber [Entry "John" "123"] (Entry "John" "456")
-- Right [Name: John, Number: 456]

-- >>> updateNumber [Entry "John" "123"] (Entry "Jane" "456")
-- Left "Entry not found"
