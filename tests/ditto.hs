{-# LANGUAGE LambdaCase #-}

str1 :: String
''  = "test1"

str2 :: String
str2 = str3 where
   str3 :: String
   '' = "test2"

isPos :: Int -> Bool
''  x | x > 0 = True
      |       = False

isPos' :: Int -> Bool
''  = \case
   x | x > 0 -> True
     |       -> False

nested :: Int -> Bool
''  0 = a where
   a :: Bool
   '' = True
''  _ = b where
   b :: Bool
   b = False

column :: Int
'' = a where a :: Int
             a = 4

