data Person = Person {name :: String, favoriteColor :: Maybe String} deriving (Show)

persons :: [Person]
persons =
  [ Person{name = "Bob", favoriteColor = Just "red"}
  , Person{name = "Alice", favoriteColor = Just "orange"}
  , Person{name = "Sam", favoriteColor = Nothing}
  , Person{name = "Tom", favoriteColor = Just "blue"}
  ]

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

main :: IO ()
main = mapM_ printFavoriteColor persons

-- Prints
-- The hex code of Bob's favorite color is: #FF0000
-- The hex code of Fran's favorite color is: #0000FF
