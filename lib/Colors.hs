module Colors where

data Person = Person {name :: String, favoriteColor :: Maybe String} deriving (Show)

-- | List of persons
persons :: [Person]
persons =
  [ Person {name = "Bob", favoriteColor = Just "red"},
    Person {name = "Alice", favoriteColor = Just "orange"},
    Person {name = "Sam", favoriteColor = Nothing},
    Person {name = "Tom", favoriteColor = Just "blue"}
  ]

-- | Function to get hex code for a color
hexForColor :: String -> Maybe String
hexForColor c = case c of
  "red" -> Just "#FF0000"
  "blue" -> Just "#0000FF"
  "yellow" -> Just "#FFFF00"
  _ -> Nothing

-- Function to print favorite color of a person
printFavoriteColor :: Person -> IO ()
printFavoriteColor person =
  case favoriteColor person >>= hexForColor of
    Just hex -> putStrLn $ "The hex code of " ++ name person ++ "'s favorite color is: " ++ hex
    Nothing -> return ()
