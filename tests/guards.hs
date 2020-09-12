{-# LANGUAGE LambdaCase #-}

isPos :: Int -> Bool
''  x | x > 0 = True
      |       = False

isPos' :: Int -> Bool
''  = \case
   x | x > 0 -> True
     |       -> False
