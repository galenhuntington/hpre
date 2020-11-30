str1 :: String
''  = "test1"

str2 :: String
str2 = str3 where
   str3 :: String
   {- comment -}
   '' = "test2"

nested :: Int -> Bool
''  0 = a where
   a :: Bool
-- comment
   '' = True
{-
   comment
-}
''  _ = b where
   b :: Bool
   b = False

column :: Int
'' = a where a :: Int
             a = 4

blankLine :: ()

''  = ()

spacesOnlyLine :: ()
          
''  = ()
